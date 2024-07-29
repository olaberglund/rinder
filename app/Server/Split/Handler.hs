module Server.Split.Handler (
    settleUpH,
    newExpenseH,
    splitPageH,
    removeExpenseH,
    saveExpenseH,
    editExpensePageH,
) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (eitherDecodeStrict, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Time              as Time
import           Data.UUID              (UUID)
import qualified Data.UUID.V4           as UUID
import           Inter.Language         (Language, mkHref)
import           Inter.Lexicon          (l)
import qualified Inter.Lexicon          as Lexicon
import           Servant                (Handler, NoContent (..), err500,
                                         throwError)
import           Server.Env             (Env (envTransactionsFile))
import           Server.Split.Html      (EditExpensePage (..),
                                         Feedback (Success),
                                         FeedbackMessage (FeedbackMessage),
                                         SplitPage (..), Transactions (..))
import           Server.Utils.Handler   (err404', hxRedirect)
import           Split                  (Expense (expenseDate, expenseId),
                                         ExpenseForm, Share, Transaction (..),
                                         findExpense, settlements, singleDebtor,
                                         toExpense)
import           System.Directory       (doesFileExist)

getTransactions :: Env -> IO (Either String [Transaction])
getTransactions env = do
    let file = envTransactionsFile env
    exists <- doesFileExist file
    unless exists (writeFile file "[]")
    res <- BS.readFile file
    pure $ case eitherDecodeStrict res of
        Right sl -> Right sl
        Left err -> Left err

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
    res <- getTransactions env
    case res of
        Right ts -> do
            let newTs = genNewTs ts
            LBS.writeFile (envTransactionsFile env) (encode newTs)
            return (Transactions lang newTs)
        Left err -> print err >> return (Transactions lang [])

splitPageH :: Env -> Language -> Handler SplitPage
splitPageH env lang = liftIO $ do
    res <- getTransactions env
    case res of
        Right ts -> return (SplitPage lang ts)
        Left err -> print err >> return (SplitPage lang [])

removeExpenseH :: Env -> Language -> UUID -> Handler NoContent
removeExpenseH env lang uuid = do
    res <- liftIO $ getTransactions env
    case res of
        Right ts -> do
            liftIO $
                LBS.writeFile
                    (envTransactionsFile env)
                    (encode (deleteExpense ts))
            hxRedirect $ encodeUtf8 $ mkHref lang "/split"
        Left err -> liftIO (print err) >> throwError err500
  where
    deleteExpense :: [Transaction] -> [Transaction]
    deleteExpense [] = []
    deleteExpense (x : xs)
        | ExpenseTransaction e <- x, uuid == expenseId e = xs
        | otherwise = x : deleteExpense xs

saveExpenseH :: Env -> Language -> UUID -> ExpenseForm -> Handler EditExpensePage
saveExpenseH env lang uuid form = do
    res <- liftIO $ getTransactions env
    case res of
        Right ts -> do
            let newTs = replaceExpense ts
            liftIO $ LBS.writeFile (envTransactionsFile env) (encode newTs)
            let mexp = findExpense uuid newTs
            case (mexp, mexp >>= singleDebtor) of
                (Just e, Just debtor) ->
                    return $ successPage e debtor
                _ -> throwError $ err404' lang (Just (l lang Lexicon.NoSuchExpense))
        Left err -> liftIO (print err) >> throwError err500
  where
    replaceExpense :: [Transaction] -> [Transaction]
    replaceExpense ((ExpenseTransaction e) : es)
        | expenseId e == uuid =
            ExpenseTransaction
                (toExpense form (expenseId e) (expenseDate e))
                : es
        | otherwise = ExpenseTransaction e : replaceExpense es
    replaceExpense (e : es) = e : replaceExpense es
    replaceExpense [] = []

    successPage :: Expense -> Share -> EditExpensePage
    successPage e debtor =
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

editExpensePageH :: Env -> Language -> UUID -> Handler EditExpensePage
editExpensePageH env lang uuid = do
    res <- liftIO $ getTransactions env
    case res of
        Right ts -> handleExpense (findExpense uuid ts)
        Left err -> printErrorAndThrow err
  where
    handleExpense (Just e) =
        case singleDebtor e of
            Just share -> return $ EditExpensePage lang e share Nothing
            Nothing -> throwError (err404' lang (Just (l lang Lexicon.MaxTwoPeople)))
    handleExpense Nothing =
        throwError $ err404' lang (Just (l lang Lexicon.NoSuchExpense))

    printErrorAndThrow err = do
        liftIO (print err)
        throwError err500
