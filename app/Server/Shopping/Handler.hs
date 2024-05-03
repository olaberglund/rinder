module Server.Shopping.Handler (
    sseH,
    removeAllH,
    removeCheckedH,
    toggleProductH,
    shoppingPageH,
    addProductH,
    productListH,
    noteProductH,
    reorderItemH,
) where

import           Control.Concurrent       (Chan, dupChan, readChan, writeChan)
import           Control.Monad            (void, when)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Newtype.Generics (Newtype, over)
import           Data.Aeson               (FromJSON (..), ToJSON, eitherDecode,
                                           eitherDecodeStrict, encode)
import qualified Data.Binary.Builder      as Builder
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Either              as Either
import qualified Data.Map                 as Map
import qualified Data.Maybe               as Maybe
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           GHC.Generics             (Generic)
import           Inter.Language           (Language)
import           Inter.Lexicon            (l)
import qualified Inter.Lexicon            as Lexicon
import           Lucid                    (Attribute, ToHtml (toHtml), renderBS)
import           Network.Wai.EventSource  (ServerEvent (..))
import           Servant                  (Handler, NoContent (..))
import           Servant.API.EventStream  (EventSource)
import qualified Servant.Types.SourceT    as S
import           Server.Env               (Env (envBroadcastChan, envShoppingListFile))
import           Server.Shopping.Html     (Checkbox (..), Direction (..),
                                           Note (noteContent, noteId),
                                           ProductSearchList (..),
                                           Reordering (..), Search (unSearch),
                                           ShoppingItem (..),
                                           ShoppingItems (..),
                                           ShoppingPage (..))
import           Store.Grocery
import qualified System.Timeout

sseH :: Env -> Handler EventSource
sseH env = liftIO $ do
    chan <- dupChan (envBroadcastChan env)
    return $ S.fromStepT (S.Yield keepAlive (rest chan))
  where
    rest :: Chan ServerEvent -> S.StepT IO ServerEvent
    rest chan = S.Effect $ do
        msg <- System.Timeout.timeout (15 * 1000000) (readChan chan)
        return $ case msg of
            Just m  -> S.Yield m (rest chan)
            Nothing -> S.Yield keepAlive (rest chan)

    keepAlive :: ServerEvent
    keepAlive = CommentEvent (Builder.fromByteString "keep-alive")

toggle :: Checkbox -> Checkbox
toggle Checked   = Unchecked
toggle Unchecked = Checked

removeAllH :: Env -> Language -> Grocery -> Handler NoContent
removeAllH env lang grocery = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right (sl :: ShoppingList) ->
            updateAndBroadCast
                env
                lang
                grocery
                (over ShoppingList (Map.adjust (const []) (groceryName grocery)) sl)
        Left err -> print err
    return NoContent

removeCheckedH :: Env -> Language -> Grocery -> Handler NoContent
removeCheckedH env lang grocery = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Left err -> print err
        Right sl ->
            case listOfGrocery grocery sl of
                Just items ->
                    let newItems = filter ((== Unchecked) . siCheck) items
                     in updateAndBroadCast
                            env
                            lang
                            grocery
                            ( over
                                ShoppingList
                                (Map.insert (groceryName grocery) newItems)
                                sl
                            )
                Nothing -> print $ "No items found for " <> groceryName grocery
    return NoContent

toggleProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
toggleProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case listOfGrocery grocery ps of
                Just items ->
                    void $
                        updateAndBroadCast
                            env
                            lang
                            grocery
                            ( over
                                ShoppingList
                                ( Map.insert
                                    (groceryName grocery)
                                    (map toggleItem items)
                                )
                                ps
                            )
                Nothing -> print $ "No items found for " <> groceryName grocery
        Left err -> print err
    return NoContent
  where
    toggleItem :: ShoppingItem -> ShoppingItem
    toggleItem i
        | siProduct i == product' = i{siCheck = toggle (siCheck i)}
        | otherwise = i

{- | Convert a non-empty list of elements with a 'ToHtml' instance to a
'ServerEvent' that can be sent to a client.

The list may not be empty, since it is not possible to return a 'raw' empty
list. To send an empty list, wrap it in an appropriate newtype.
-}
asServerEvent :: (ToHtml a) => Maybe Text -> [a] -> ServerEvent
asServerEvent msgName =
    ServerEvent (Builder.fromByteString . encodeUtf8 <$> msgName) Nothing
        . map (Builder.fromLazyByteString . renderBS . toHtml)

shoppingPageH :: Env -> Language -> Grocery -> Handler ShoppingPage
shoppingPageH env lang grocery = liftIO $ do
    fetchedPromotions <- groceryGetOffers grocery
    shoppingItems <- eitherDecode <$> LBS.readFile (envShoppingListFile env)
    case shoppingItems of
        Left err ->
            putStrLn err
                >> return
                    ( ShoppingPage
                        lang
                        mempty
                        (groceryName grocery)
                        mempty
                        (Nothing)
                    )
        Right list ->
            case listOfGrocery grocery list of
                Nothing -> do
                    print $ "Creating new shopping list for " <> groceryName grocery
                    let newList =
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) [])
                                list
                    void $ updateAndBroadCast env lang grocery newList
                    return
                        ( ShoppingPage
                            lang
                            mempty
                            (groceryName grocery)
                            (Either.fromRight mempty fetchedPromotions)
                            (Just [])
                        )
                Just items ->
                    return
                        ( ShoppingPage
                            lang
                            mempty
                            (groceryName grocery)
                            (Either.fromRight mempty fetchedPromotions)
                            (Just items)
                        )

addProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
addProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case listOfGrocery grocery ps of
                Just items -> updateList ps items
                Nothing    -> pure ()
        Left err -> print err
    return NoContent
  where
    updateList :: ShoppingList -> [ShoppingItem] -> IO ()
    updateList ps items = when (product' `notElem` map siProduct items) $ do
        let newList = ShoppingItem product' Unchecked "" : items
            shoppingList =
                over
                    ShoppingList
                    (Map.insert (groceryName grocery) newList)
                    ps
        updateAndBroadCast env lang grocery shoppingList

listOfGrocery :: Grocery -> ShoppingList -> Maybe [ShoppingItem]
listOfGrocery grocery list =
    Map.lookup
        (groceryName grocery)
        (unShoppingList list)

reorderItemH :: Env -> Language -> Grocery -> Reordering -> Handler NoContent
reorderItemH env lang grocery (Reordering pId dir) = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case listOfGrocery grocery ps of
                Just items ->
                    updateAndBroadCast env lang grocery (reorderedList ps items)
                Nothing -> pure ()
        Left err -> print err
    return NoContent
  where
    reorderedList :: ShoppingList -> [ShoppingItem] -> ShoppingList
    reorderedList ps items =
        over
            ShoppingList
            (Map.insert (groceryName grocery) (reorder items))
            ps

    reorder :: [ShoppingItem] -> [ShoppingItem]
    reorder (x : y : xs)
        | productId (siProduct x) == pId && dir == Down = y : x : xs
        | productId (siProduct y) == pId && dir == Up = y : x : xs
        | otherwise = x : reorder (y : xs)
    reorder xs = xs

noteProductH :: Env -> Language -> Grocery -> Note -> Handler NoContent
noteProductH env lang grocery note = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case listOfGrocery grocery ps of
                Just items ->
                    let newList = map modifyNote items
                        shoppingList =
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) newList)
                                ps
                     in updateAndBroadCast env lang grocery shoppingList
                Nothing -> pure ()
        Left err -> print err
    return NoContent
  where
    modifyNote :: ShoppingItem -> ShoppingItem
    modifyNote i
        | productId (siProduct i) == noteId note = i{siNote = noteContent note}
        | otherwise = i

updateAndBroadCast :: Env -> Language -> Grocery -> ShoppingList -> IO ()
updateAndBroadCast env lang grocery items =
    LBS.writeFile (envShoppingListFile env) (encode items)
        >> writeChan
            (envBroadcastChan env)
            ( asServerEvent
                (Just (groceryName grocery))
                [ ShoppingItems
                    lang
                    (groceryName grocery)
                    ( Maybe.fromMaybe
                        []
                        (unShoppingList items Map.!? groceryName grocery)
                    )
                ]
            )

productListH ::
    Language ->
    Grocery ->
    (Product -> [Attribute]) ->
    Search ->
    Handler ProductSearchList
productListH lang grocery attributes search = liftIO $ do
    res <- groceryGetSearchProduct grocery (unSearch search)
    case res of
        Left err ->
            print err
                >> return
                    ( ProductSearchList
                        lang
                        mempty
                        mempty
                        (l lang Lexicon.SearchResults)
                        "searched-products"
                    )
        Right products ->
            return $
                ProductSearchList
                    lang
                    attributes
                    products
                    (l lang Lexicon.SearchResults)
                    "searched-products"

newtype ShoppingList = ShoppingList
    { unShoppingList :: Map.Map Text [ShoppingItem]
    }
    deriving stock (Generic, Show)
    deriving newtype (ToJSON, FromJSON)
    deriving anyclass (Newtype)
