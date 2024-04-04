{- |  This module contains the handlers for the server. The handlers are
responsible for handling the requests and returning the appropriate responses.
-}
module Server.Handler (
    addProductH,
    customFormatters,
    editExpensePageH,
    newExpenseH,
    productListH,
    redirect,
    removeAllH,
    removeCheckedH,
    removeExpenseH,
    saveExpenseH,
    settleUpH,
    shoppingPageH,
    splitPageH,
    sseH,
    toggleProductH,
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
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Time qualified as Time
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Inter.Language (Language)
import Inter.Lexicon (l)
import Inter.Lexicon qualified as Lexicon
import Lucid (Attribute, ToHtml (toHtml), renderBS)
import Network.HTTP.Types (hLocation)
import Network.Wai.EventSource (ServerEvent (..))
import Servant (
    ErrorFormatters (notFoundErrorFormatter),
    Handler,
    NoContent (..),
    ServerError (errBody, errHeaders),
    err303,
    err404,
    err500,
    throwError,
 )
import Servant.API.EventStream (EventSource)
import Servant.Server (defaultErrorFormatters)
import Servant.Types.SourceT qualified as S
import Server.Env
import Server.Html
import Split
import System.Timeout qualified
import Willys.Client
import Willys.Response

customFormatters :: Language -> ErrorFormatters
customFormatters lang =
    defaultErrorFormatters
        { notFoundErrorFormatter = const (err404' lang Nothing)
        }

err404' :: Language -> Maybe Text -> ServerError
err404' lang msg =
    err404
        { errBody =
            renderBS $
                toHtml (Page404 lang (Maybe.fromMaybe "Inget att se här..." msg))
        }

removeExpenseH :: Env -> UUID -> Handler NoContent
removeExpenseH env uuid = do
    res <- liftIO $ BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            liftIO $
                LBS.writeFile
                    (envTransactionsFile env)
                    (encode (deleteExpense ts))
            hxRedirect "/split"
        Left err -> liftIO (print err) >> throwError err500
  where
    deleteExpense :: [Transaction] -> [Transaction]
    deleteExpense [] = []
    deleteExpense (x : xs)
        | ExpenseTransaction e <- x, uuid == expenseId e = xs
        | otherwise = x : deleteExpense xs

saveExpenseH :: Env -> Language -> UUID -> ExpenseForm -> Handler EditExpensePage
saveExpenseH env lang uuid form = do
    res <- liftIO $ BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            let newTs = map replaceExpense ts
            liftIO $ LBS.writeFile (envTransactionsFile env) (encode newTs)
            let mexp = findExpense uuid newTs
            case (mexp, mexp >>= singleDebtor) of
                (Just e, Just debtor) ->
                    return $
                        EditExpensePage
                            lang
                            e
                            debtor
                            ( Just
                                ( FeedbackMessage
                                    Success
                                    (l lang Lexicon.ExpenseSaved)
                                )
                            )
                _ ->
                    throwError $
                        err404'
                            lang
                            (Just (l lang Lexicon.NoSuchExpense))
        Left err -> liftIO (print err) >> throwError err500
  where
    replaceExpense :: Transaction -> Transaction
    replaceExpense (ExpenseTransaction e) =
        if expenseId e == uuid
            then
                ExpenseTransaction
                    (toExpense form (expenseId e) (expenseDate e))
            else ExpenseTransaction e
    replaceExpense t = t

editExpensePageH :: Env -> Language -> UUID -> Handler EditExpensePage
editExpensePageH env lang uuid = do
    res <- liftIO $ BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            let expense = findExpense uuid ts
            case expense of
                Just e -> case singleDebtor e of
                    Just share -> return $ EditExpensePage lang e share Nothing
                    Nothing ->
                        throwError $
                            err404'
                                lang
                                (Just (l lang Lexicon.MaxTwoPeople))
                Nothing ->
                    throwError $
                        err404' lang (Just (l lang Lexicon.NoSuchExpense))
        Left err -> liftIO (print err) >> throwError err500
  where

settleUpH :: Env -> Language -> Handler Transactions
settleUpH env lang = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    writeTransactionsHandlerHelper
        env
        lang
        (\ts -> map SettlementTransaction (settlements tz utc ts) <> ts)

newExpenseH :: Env -> Language -> ExpenseForm -> Handler Transactions
newExpenseH env lang form = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    uuid <- liftIO UUID.nextRandom
    writeTransactionsHandlerHelper
        env
        lang
        ( ExpenseTransaction
            (toExpense form uuid (Time.utcToLocalTime tz utc))
            :
        )

writeTransactionsHandlerHelper ::
    Env ->
    Language ->
    ([Transaction] -> [Transaction]) ->
    Handler Transactions
writeTransactionsHandlerHelper env lang genNewTs = liftIO $ do
    res <- BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            let newTs = genNewTs ts
            LBS.writeFile (envTransactionsFile env) (encode newTs)
            return (Transactions lang newTs)
        Left err -> print err >> return (Transactions lang [])

splitPageH :: Env -> Language -> Handler SplitPage
splitPageH env lang = liftIO $ do
    res <- BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> return (SplitPage lang ts)
        Left err -> print err >> return (SplitPage lang [])

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

hxRedirect :: BS.ByteString -> Handler a
hxRedirect url = throwError err303{errHeaders = [("HX-Redirect", url)]}

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303{errHeaders = [(hLocation, url)]}

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
