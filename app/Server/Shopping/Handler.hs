module Server.Shopping.Handler (
    sseH,
    removeAllH,
    removeCheckedH,
    toggleProductH,
    shoppingPageH,
    addProductH,
    productListH,
) where

import Control.Concurrent (Chan, dupChan, readChan, writeChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
    eitherDecode,
    eitherDecodeStrict,
    encode,
 )
import Data.Binary.Builder qualified as Builder
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either qualified as Either
import Data.List.NonEmpty qualified as NonEmpty
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
import Server.Env
import Server.Shopping.Html (
    Checkbox (..),
    ProductSearchList (..),
    Search (unSearch),
    ShoppingItem (..),
    ShoppingItems (ShoppingItems),
    ShoppingPage (..),
 )
import Store.Willys.Client
import Store.Willys.Response
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

removeAllH :: Env -> Handler ShoppingItems
removeAllH env = liftIO $ updateAndBroadCast env []

removeCheckedH :: Env -> Handler ShoppingItems
removeCheckedH env = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newItems = filter ((== Unchecked) . siCheck) ps
             in updateAndBroadCast env newItems
        Left err -> print err >> return (ShoppingItems [])

toggleProductH :: Env -> Product -> Handler NoContent
toggleProductH env product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps -> void $ updateAndBroadCast env (map toggleItem ps)
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
asServerEvent :: (ToHtml a) => NonEmpty.NonEmpty a -> ServerEvent
asServerEvent =
    ServerEvent Nothing Nothing
        . map (Builder.fromLazyByteString . renderBS . toHtml)
        . NonEmpty.toList

shoppingPageH :: Env -> Language -> Handler ShoppingPage
shoppingPageH env lang = liftIO $ do
    fetchedPromotions <- runClientDefault fetchPromotions
    shoppingItems <- eitherDecode <$> LBS.readFile (envShoppingListFile env)
    case shoppingItems of
        Left err ->
            putStrLn err
                >> return (ShoppingPage lang mempty mempty mempty)
        Right list ->
            return
                ( ShoppingPage
                    lang
                    mempty
                    (Either.fromRight mempty fetchedPromotions)
                    (Just list)
                )

addProductH :: Env -> Product -> Handler ShoppingItems
addProductH env product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newList = ShoppingItem product' Unchecked : ps
             in updateAndBroadCast env newList
        Left err -> print err >> return (ShoppingItems [])

updateAndBroadCast :: Env -> [ShoppingItem] -> IO ShoppingItems
updateAndBroadCast env items =
    LBS.writeFile (envShoppingListFile env) (encode items)
        >> writeChan
            (envBroadcastChan env)
            (asServerEvent (NonEmpty.singleton (ShoppingItems items)))
        >> return (ShoppingItems items)

productListH ::
    Language ->
    (Product -> [Attribute]) ->
    Search ->
    Handler ProductSearchList
productListH lang attributes search = liftIO $ do
    res <- runClientDefault (fetchProducts (unSearch search))
    case res of
        Left err ->
            print err
                >> return
                    ( ProductSearchList
                        mempty
                        mempty
                        (l lang Lexicon.SearchResults)
                        "searched-products"
                    )
        Right products ->
            return $
                ProductSearchList
                    attributes
                    products
                    (l lang Lexicon.SearchResults)
                    "searched-products"
