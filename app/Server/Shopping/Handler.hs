module Server.Shopping.Handler (
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
import           Control.Monad            (unless, void)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Newtype.Generics (Newtype, over)
import           Data.Aeson               (FromJSON (..), ToJSON,
                                           eitherDecodeStrict, encode)
import qualified Data.Binary.Builder      as Builder
import qualified Data.ByteString          as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Either              as Either
import           Data.Foldable            (for_)
import qualified Data.Map                 as Map
import qualified Data.Maybe               as Maybe
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.Encoding       (encodeUtf8)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           GHC.Generics             (Generic)
import           Inter.Language           (Language)
import           Inter.Lexicon            (l)
import qualified Inter.Lexicon            as Lexicon
import           Lucid                    (Attribute, ToHtml (toHtml), renderBS)
import           Network.Wai.EventSource  (ServerEvent (..))
import           Servant                  (Handler, NoContent (..),
                                           ServerError (..), err404, err500,
                                           throwError)
import qualified Servant.Types.SourceT    as S
import           Server.Env               (Env (envShoppingListFile),
                                           envKeepAliveChan)
import           Server.Shopping.Html     (Checkbox (..), Direction (..),
                                           Note (noteContent, noteId),
                                           ProductSearchList (..),
                                           Reordering (..), Search (unSearch),
                                           ShoppingItem (..),
                                           ShoppingItems (..),
                                           ShoppingPage (..))
import           Store.Grocery
import           System.Directory         (doesFileExist)
import qualified System.Timeout

getShoppingList :: Env -> IO (Either String ShoppingList)
getShoppingList env = do
    let file = envShoppingListFile env
    exists <- doesFileExist file
    unless exists (writeFile file "[]")
    res <- BS.readFile file
    pure $ case eitherDecodeStrict res of
        Right sl -> Right sl
        Left err -> Left err

-- sseH :: Env -> Handler EventSource
-- sseH env = liftIO $ do
--     chan <- dupChan $ envKeepAliveChan env
--     return $ S.fromStepT (S.Yield keepAlive (rest chan))
--   where
--     rest :: Chan ServerEvent -> S.StepT IO ServerEvent
--     rest chan = S.Effect $ do
--         msg <- System.Timeout.timeout (15 * 1_000_000) (readChan chan)
--         return $ case msg of
--             Just m  -> S.Yield m (rest chan)
--             Nothing -> S.Yield keepAlive (rest chan)
--
--     keepAlive :: ServerEvent
--     keepAlive = CommentEvent (Builder.fromByteString "keep-alive")

toggle :: Checkbox -> Checkbox
toggle Checked   = Unchecked
toggle Unchecked = Checked

removeAllH :: Env -> Language -> Grocery -> Handler NoContent
removeAllH env lang grocery = do
    list <- liftIO $ getShoppingList env
    case list of
        Left err -> throwError err500{errBody = TL.encodeUtf8 . TL.pack $ "Error: " <> err}
        Right list' -> do
            liftIO $ updateAndBroadCast env lang grocery (removeAll list')
            pure NoContent
  where
    removeAll :: ShoppingList -> ShoppingList
    removeAll = over ShoppingList (Map.adjust (const []) (groceryName grocery))

removeCheckedH :: Env -> Language -> Grocery -> Handler NoContent
removeCheckedH env lang grocery = do
    list <- liftIO $ getShoppingList env
    case list of
        Left err -> throwError err500{errBody = TL.encodeUtf8 . TL.pack $ "Error: " <> err}
        Right list' -> do
            liftIO $ removeChecked list'
            pure NoContent
  where
    removeChecked sl = case listOfGrocery grocery sl of
        Just items ->
            let newItems = filter ((== Unchecked) . siCheck) items
             in updateAndBroadCast env lang grocery (over ShoppingList (Map.insert (groceryName grocery) newItems) sl)
        Nothing -> print $ "No items found for " <> groceryName grocery

toggleProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
toggleProductH env lang grocery product' = do
    list <- liftIO $ getShoppingList env
    case list of
        Right list' -> handleList list'
        Left err    -> throwError err500{errBody = encodeErr "Error: " err}
  where
    handleList :: ShoppingList -> Handler NoContent
    handleList list' =
        case listOfGrocery grocery list' of
            Just items -> updateItems items list'
            Nothing -> throwError err404{errBody = encodeErr "No items found for " (Text.unpack (groceryName grocery))}

    updateItems :: [ShoppingItem] -> ShoppingList -> Handler NoContent
    updateItems items list' = do
        let newItems = map toggleItem items
        liftIO $ updateAndBroadCast env lang grocery (over ShoppingList (Map.insert (groceryName grocery) newItems) list')
        pure NoContent

    toggleItem :: ShoppingItem -> ShoppingItem
    toggleItem i
        | siProduct i == product' = i{siCheck = toggle (siCheck i)}
        | otherwise = i

    encodeErr :: String -> String -> ByteString
    encodeErr msg err = TL.encodeUtf8 . TL.pack $ msg <> err

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
    shoppingItems <- getShoppingList env
    handleShoppingItems fetchedPromotions shoppingItems
  where
    handleShoppingItems :: Either a [Product] -> Either String ShoppingList -> IO ShoppingPage
    handleShoppingItems fetchedPromotions shoppingItems =
        case shoppingItems of
            Left err   -> handleShoppingItemsError err
            Right list -> handleShoppingList fetchedPromotions list

    handleShoppingItemsError :: String -> IO ShoppingPage
    handleShoppingItemsError err = do
        putStrLn err
        return $ ShoppingPage lang mempty (groceryName grocery) mempty Nothing

    handleShoppingList :: Either a [Product] -> ShoppingList -> IO ShoppingPage
    handleShoppingList fetchedPromotions list =
        case listOfGrocery grocery list of
            Nothing -> do
                createNewShoppingList list
                return (mkShoppingPage fetchedPromotions [])
            Just items -> return $ mkShoppingPage fetchedPromotions items

    createNewShoppingList :: ShoppingList -> IO ()
    createNewShoppingList list = do
        print $ "Creating new shopping list for " <> groceryName grocery
        let newList = over ShoppingList (Map.insert (groceryName grocery) []) list
        void $ updateAndBroadCast env lang grocery newList

    mkShoppingPage :: Either a [Product] -> [ShoppingItem] -> ShoppingPage
    mkShoppingPage fetchedPromotions items =
        ShoppingPage
            lang
            mempty
            (groceryName grocery)
            (Either.fromRight mempty fetchedPromotions)
            (Just items)

addProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
addProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            for_ (listOfGrocery grocery ps) (updateList ps)
        Left err -> print err
    return NoContent
  where
    updateList :: ShoppingList -> [ShoppingItem] -> IO ()
    updateList ps items = unless (product' `elem` map siProduct items) $ do
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
        Right ps -> handleShoppingList ps
        Left err -> print err
    return NoContent
  where
    handleShoppingList :: ShoppingList -> IO ()
    handleShoppingList ps =
        case listOfGrocery grocery ps of
            Just items -> updateAndBroadCast env lang grocery (updateItems items ps)
            Nothing -> pure ()

    updateItems :: [ShoppingItem] -> ShoppingList -> ShoppingList
    updateItems = over ShoppingList . Map.insert (groceryName grocery) . map modifyNote

    modifyNote :: ShoppingItem -> ShoppingItem
    modifyNote i
        | productId (siProduct i) == noteId note = i{siNote = noteContent note}
        | otherwise = i

updateAndBroadCast :: Env -> Language -> Grocery -> ShoppingList -> IO ()
updateAndBroadCast env lang grocery items = do
    LBS.writeFile (envShoppingListFile env) (encode items)
    writeChan (envKeepAliveChan env) serverEvent
  where
    serverEvent :: ServerEvent
    serverEvent =
        asServerEvent (Just (groceryName grocery)) [items']

    items' :: ShoppingItems
    items' = ShoppingItems lang (groceryName grocery) getGroceryItems

    getGroceryItems :: [ShoppingItem]
    getGroceryItems =
        Maybe.fromMaybe [] (unShoppingList items Map.!? groceryName grocery)

productListH ::
    Language ->
    Grocery ->
    (Product -> [Attribute]) ->
    Search ->
    Handler ProductSearchList
productListH lang grocery attributes search = liftIO $ do
    res <- groceryGetSearchProduct grocery (unSearch search)
    case res of
        Left err       -> print err >> return emptySearchList
        Right products -> return $ mkProductSearchList products
  where
    mkProductSearchList :: [Product] -> ProductSearchList
    mkProductSearchList products =
        ProductSearchList
            lang
            attributes
            products
            (l lang Lexicon.SearchResults)
            "searched-products"

    emptySearchList :: ProductSearchList
    emptySearchList =
        ProductSearchList
            lang
            mempty
            mempty
            (l lang Lexicon.SearchResults)
            "searched-products"

newtype ShoppingList = ShoppingList
    { unShoppingList :: Map.Map Text [ShoppingItem]
    }
    deriving stock (Generic, Show)
    deriving newtype (ToJSON, FromJSON)
    deriving anyclass (Newtype)
