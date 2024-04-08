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

import Control.Concurrent (Chan, dupChan, readChan, writeChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Newtype.Generics (Newtype, over)
import Data.Aeson (
    FromJSON (..),
    ToJSON,
    eitherDecode,
    eitherDecodeStrict,
    encode,
 )
import Data.Binary.Builder qualified as Builder
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either qualified as Either
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Inter.Language (Language)
import Inter.Lexicon (l)
import Inter.Lexicon qualified as Lexicon
import Lucid (Attribute, ToHtml (toHtml), renderBS)
import Network.Wai.EventSource (ServerEvent (..))
import Servant (
    Handler,
    NoContent (..),
 )
import Servant.API.EventStream (EventSource)
import Servant.Types.SourceT qualified as S
import Server.Env (Env (envBroadcastChan, envShoppingListFile))
import Server.Shopping.Html (
    Checkbox (..),
    Direction (..),
    Note (noteContent, noteId),
    ProductSearchList (..),
    Reordering (..),
    Search (unSearch),
    ShoppingItem (..),
    ShoppingItems (..),
    ShoppingPage (..),
 )
import Store.Grocery
import System.Timeout qualified

sseH :: Env -> Handler EventSource
sseH env = liftIO $ do
    chan <- dupChan (envBroadcastChan env)
    return $ S.fromStepT (S.Yield keepAlive (rest chan))
  where
    rest :: Chan ServerEvent -> S.StepT IO ServerEvent
    rest chan = S.Effect $ do
        msg <- System.Timeout.timeout (15 * 1000000) (readChan chan)
        return $ case msg of
            Just m -> S.Yield m (rest chan)
            Nothing -> S.Yield keepAlive (rest chan)

    keepAlive :: ServerEvent
    keepAlive = CommentEvent (Builder.fromByteString "keep-alive")

toggle :: Checkbox -> Checkbox
toggle Checked = Unchecked
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
        Left err -> print err >> return NoContent

removeCheckedH :: Env -> Language -> Grocery -> Handler NoContent
removeCheckedH env lang grocery = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right sl ->
            case Map.lookup (groceryName grocery) (unShoppingList sl) of
                Just items ->
                    let newItems = filter ((== Unchecked) . siCheck) items
                     in updateAndBroadCast env lang grocery $
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) newItems)
                                sl
                Nothing -> return NoContent
        Left err -> print err >> return NoContent

toggleProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
toggleProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case Map.lookup (groceryName grocery) (unShoppingList ps) of
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
            case Map.lookup (groceryName grocery) (unShoppingList list) of
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
            case Map.lookup (groceryName grocery) (unShoppingList ps) of
                Just items ->
                    let newList = ShoppingItem product' Unchecked "" : items
                        shoppingList =
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) newList)
                                ps
                     in updateAndBroadCast env lang grocery shoppingList
                Nothing -> return NoContent
        Left err -> print err >> return NoContent

-- Reordering (productid) (direction )
reorderItemH :: Env -> Language -> Grocery -> Reordering -> Handler NoContent
reorderItemH env lang grocery (Reordering pId dir) = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case Map.lookup (groceryName grocery) (unShoppingList ps) of
                Just items ->
                    let newList = reorderItem items
                        shoppingList =
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) newList)
                                ps
                     in updateAndBroadCast env lang grocery shoppingList
                Nothing -> return NoContent
        Left err -> print err >> return NoContent
  where
    reorderItem :: [ShoppingItem] -> [ShoppingItem]
    reorderItem (x : y : xs)
        | productId (siProduct x) == pId && dir == Down = y : x : xs
        | productId (siProduct y) == pId && dir == Up = y : x : xs
        | otherwise = x : reorderItem (y : xs)
    reorderItem xs = xs

noteProductH :: Env -> Language -> Grocery -> Note -> Handler NoContent
noteProductH env lang grocery note = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            case Map.lookup (groceryName grocery) (unShoppingList ps) of
                Just items ->
                    let newList = map modifyNote items
                        shoppingList =
                            over
                                ShoppingList
                                (Map.insert (groceryName grocery) newList)
                                ps
                     in updateAndBroadCast env lang grocery shoppingList
                Nothing -> return NoContent
        Left err -> print err >> return NoContent
  where
    modifyNote :: ShoppingItem -> ShoppingItem
    modifyNote i
        | productId (siProduct i) == noteId note = i{siNote = noteContent note}
        | otherwise = i

updateAndBroadCast :: Env -> Language -> Grocery -> ShoppingList -> IO NoContent
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
        >> return NoContent

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
