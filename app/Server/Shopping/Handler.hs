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
    ProductSearchList (..),
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
removeAllH env lang grocery = liftIO $ updateAndBroadCast env lang grocery []

removeCheckedH :: Env -> Language -> Grocery -> Handler NoContent
removeCheckedH env lang grocery = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newItems = filter ((== Unchecked) . siCheck) ps
             in updateAndBroadCast env lang grocery newItems
        Left err -> print err >> return NoContent

toggleProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
toggleProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps -> void $ updateAndBroadCast env lang grocery (map toggleItem ps)
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
asServerEvent :: (ToHtml a) => [a] -> ServerEvent
asServerEvent =
    ServerEvent Nothing Nothing
        . map (Builder.fromLazyByteString . renderBS . toHtml)

shoppingPageH :: Env -> Language -> Grocery -> Handler ShoppingPage
shoppingPageH env lang grocery = liftIO $ do
    fetchedPromotions <- groceryGetOffers grocery
    shoppingItems <- eitherDecode <$> LBS.readFile (envShoppingListFile env)
    case shoppingItems of
        Left err ->
            putStrLn err
                >> return (ShoppingPage lang mempty (groceryName grocery) mempty (Nothing))
        Right list ->
            return
                ( ShoppingPage
                    lang
                    mempty
                    (groceryName grocery)
                    (Either.fromRight mempty fetchedPromotions)
                    (Just list)
                )

addProductH :: Env -> Language -> Grocery -> Product -> Handler NoContent
addProductH env lang grocery product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newList = ShoppingItem product' Unchecked : ps
             in updateAndBroadCast env lang grocery newList
        Left err -> print err >> return NoContent

updateAndBroadCast :: Env -> Language -> Grocery -> [ShoppingItem] -> IO NoContent
updateAndBroadCast env lang grocery items =
    LBS.writeFile (envShoppingListFile env) (encode items)
        >> writeChan
            (envBroadcastChan env)
            (asServerEvent [ShoppingItems lang (groceryName grocery) items])
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
