module Server.Handler where

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
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Time qualified as Time
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
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
import Splitvajs
import System.Timeout qualified
import Willys.Client
import Willys.Response

customFormatters :: ErrorFormatters
customFormatters =
    defaultErrorFormatters
        { notFoundErrorFormatter = const (err404' Nothing)
        }

err404' :: Maybe Text -> ServerError
err404' msg =
    err404
        { errBody =
            renderBS $
                toHtml (Page404 (Maybe.fromMaybe "Inget att se här..." msg))
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

saveExpenseH :: Env -> UUID -> ExpenseForm -> Handler EditExpensePage
saveExpenseH env uuid form = do
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
                            e
                            debtor
                            (Just (FeedbackMessage Success "Utgift sparad"))
                _ -> throwError $ err404' (Just "Ingen sådan utgift hittades")
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

editExpensePageH :: Env -> UUID -> Handler EditExpensePage
editExpensePageH env uuid = do
    res <- liftIO $ BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            let expense = findExpense uuid ts
            case expense of
                Just e -> case singleDebtor e of
                    Just share -> return $ EditExpensePage e share Nothing
                    Nothing -> throwError $ err404' (Just moreThanTwoError)
                Nothing ->
                    throwError $
                        err404' (Just "Ingen sådan utgift hittades")
        Left err -> liftIO (print err) >> throwError err500
  where
    moreThanTwoError =
        "Just nu stöds inte \
        \redigering av utgifter med fler deltagare än två"

settleUpH :: Env -> Handler Transactions
settleUpH env = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    writeTransactionsHandlerHelper
        env
        (\ts -> map SettlementTransaction (settlements tz utc ts) <> ts)

newExpenseH :: Env -> ExpenseForm -> Handler Transactions
newExpenseH env form = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    uuid <- liftIO UUID.nextRandom
    writeTransactionsHandlerHelper
        env
        ( ExpenseTransaction
            (toExpense form uuid (Time.utcToLocalTime tz utc))
            :
        )

writeTransactionsHandlerHelper ::
    Env ->
    ([Transaction] -> [Transaction]) ->
    Handler Transactions
writeTransactionsHandlerHelper env genNewTs = liftIO $ do
    res <- BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> do
            let newTs = genNewTs ts
            LBS.writeFile (envTransactionsFile env) (encode newTs)
            return (Transactions newTs)
        Left err -> print err >> return (Transactions [])

splitPageH :: Env -> Handler SplitPage
splitPageH env = liftIO $ do
    res <- BS.readFile (envTransactionsFile env)
    case eitherDecodeStrict res of
        Right ts -> return (SplitPage ts)
        Left err -> print err >> return (SplitPage [])

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

removeAllH :: Env -> Handler [ShoppingItem]
removeAllH env =
    liftIO $
        LBS.writeFile (envShoppingListFile env) "[]"
            >> return []

removeCheckedH :: Env -> Handler [ShoppingItem]
removeCheckedH env = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newItems = filter ((== Unchecked) . siCheck) ps
             in updateAndBroadCast env newItems
        Left err -> print err >> return []

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

asServerEvent :: (ToHtml a) => [a] -> ServerEvent
asServerEvent =
    ServerEvent Nothing Nothing
        . map (Builder.fromLazyByteString . renderBS . toHtml)

shoppingPageH :: Env -> Handler ShoppingPage
shoppingPageH env = liftIO $ do
    fetchedPromotions <- runClientDefault fetchPromotions
    shoppingItems <- eitherDecode <$> LBS.readFile (envShoppingListFile env)
    case shoppingItems of
        Left err -> putStrLn err >> return (ShoppingPage mempty mempty mempty)
        Right list ->
            return
                ( ShoppingPage
                    mempty
                    (Either.fromRight mempty fetchedPromotions)
                    (Just list)
                )

addProductH :: Env -> Product -> Handler [ShoppingItem]
addProductH env product' = liftIO $ do
    res <- BS.readFile (envShoppingListFile env)
    case eitherDecodeStrict res of
        Right ps ->
            let newList = ShoppingItem product' Unchecked : ps
             in updateAndBroadCast env newList
        Left err -> print err >> return []

hxRedirect :: BS.ByteString -> Handler a
hxRedirect url = throwError err303{errHeaders = [("HX-Redirect", url)]}

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303{errHeaders = [(hLocation, url)]}

updateAndBroadCast :: Env -> [ShoppingItem] -> IO [ShoppingItem]
updateAndBroadCast env items =
    LBS.writeFile (envShoppingListFile env) (encode items)
        >> writeChan (envBroadcastChan env) (asServerEvent items)
        >> return items

productListH ::
    (Product -> [Attribute]) ->
    Search ->
    Handler ProductSearchList
productListH attributes search = liftIO $ do
    res <- runClientDefault (fetchProducts (unSearch search))
    case res of
        Left err ->
            print err
                >> return
                    ( ProductSearchList
                        mempty
                        mempty
                        "Sökresultat"
                        "searched-products"
                    )
        Right products ->
            return $
                ProductSearchList
                    attributes
                    products
                    "Sökresultat"
                    "searched-products"
