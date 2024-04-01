module Splitvajs where

import Control.Arrow (second)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.List (find, foldl', nub, partition, sortOn, unfoldr)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime, TimeZone, UTCTime, defaultTimeLocale, formatTime, utcToLocalTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Lucid
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)
import Willys (safeHead)
import Prelude hiding (exp, product)

data Person = Person {name :: Text, color :: Text}
  deriving (Generic, Ord, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance Show Person where
  show = T.unpack . name

instance ToHtml Person where
  toHtml person = toHtml $ person.name
  toHtmlRaw = toHtml

people :: [Person]
people = [wilma, ola]

ola :: Person
ola = Person "Ola" "RoyalBlue"

wilma :: Person
wilma = Person "Wilma" "DarkRed"

-- | Pay off debts
-- | Takes a list of people with positive credit and a debt.
-- | Returns a list of IOUs and a list of people with remaining credit.
payOff :: [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
payOff credits debts = payOff' [] credits debts
  where
    payOff' :: [IOU] -> [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
    payOff' ds (Tally p credit : ts) (Tally p' debt) = case compare credit debt of
      GT -> (IOU p' p debt : ds, Tally p (credit - debt) : ts)
      EQ -> (IOU p' p credit : ds, ts)
      LT -> payOff' (IOU p' p credit : ds) ts (Tally p' (debt - credit))
    payOff' _ [] _ = ([], [])

simplifiedDebts :: [Transaction] -> IOUMap
simplifiedDebts = uncurry minimizeTransactions . tallies

debtsToList :: IOUMap -> [(Person, [(Person, Amount)])]
debtsToList = M.toList . M.map M.toList . coerce

minimizeTransactions :: [Tally Owed] -> [Tally Owing] -> IOUMap
minimizeTransactions credits debts = IOUMap $ M.unions @[] $ coerce $ unfoldr payOffStep (credits, debts)
  where
    payOffStep :: ([Tally Owed], [Tally Owing]) -> Maybe (IOUMap, ([Tally Owed], [Tally Owing]))
    payOffStep (cs, d : ds) = let (ious', cs') = payOff cs d in Just (iousToMap ious', (cs', ds))
    payOffStep _ = Nothing

type Amount = Float

data Transaction = ExpenseTransaction Expense | SettlementTransaction Settlement
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Expense = Expense
  { id :: UUID,
    split :: Split,
    total :: Amount,
    paidBy :: Person,
    rubric :: Text,
    date :: LocalTime
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Settlement = Settlement
  { from :: Person,
    to :: Person,
    amount :: Amount,
    date :: LocalTime
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | the current implementation of the expense form
data ExpenseForm = ExpenseForm
  { debtor :: Person,
    paidBy :: Person,
    split :: Split,
    shareType :: ShareType,
    amount :: Float,
    rubric :: Text,
    total :: Amount
  }

instance FromForm ExpenseForm where
  fromForm form = do
    debtor <- (parseUnique "debtor" form >>= findPerson)
    paidBy <- (parseUnique "paidBy" form >>= findPerson)
    total <- (parseUnique "total" form >>= mayNotBeNegative "Total")
    rubric <- (parseUnique "rubric" form >>= mayNotBeEmpty)
    amount <- (parseUnique "amount" form >>= mayNotBeNegative "Amount")
    shareType <- fromForm form
    oPerson <- note "Couldn't find person" (otherPerson debtor)
    split' <- case shareType of
      Percentage ->
        if amount > 100
          then Left "Percentage may not be greater than 100"
          else mkSplit total [Share Percentage debtor amount True, Share Percentage oPerson (100 - amount) False]
      Fixed -> mkSplit total [Share Fixed debtor amount True, Share Fixed oPerson (total - amount) False]
    pure $ ExpenseForm debtor paidBy split' shareType amount rubric total
    where
      findPerson :: Text -> Either Text Person
      findPerson n = maybe (Left $ "Person " <> n <> " not found") Right $ find ((== n) . name) people

      otherPerson :: Person -> Maybe Person
      otherPerson p = safeHead $ filter (/= p) people

      mayNotBeNegative :: Text -> Float -> Either Text Float
      mayNotBeNegative prop a = if a >= 0 then Right a else Left $ prop <> " may not be negative"

      mayNotBeEmpty :: Text -> Either Text Text
      mayNotBeEmpty rubric = if T.null rubric then Left "Rubric may not be empty" else Right rubric

      note :: Text -> Maybe a -> Either Text a
      note e = maybe (Left e) Right

toExpense :: ExpenseForm -> UUID -> LocalTime -> Expense
toExpense form id' localTime = Expense id' form.split form.total form.paidBy form.rubric localTime

data ShareType = Percentage | Fixed
  deriving (Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance ToHtml ShareType where
  toHtml Fixed = "kr"
  toHtml Percentage = "%"
  toHtmlRaw = toHtml

data Share = Share {shareType :: ShareType, person :: Person, amount :: Float, entered :: Bool}
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Show ShareType where
  show Percentage = "percentage"
  show Fixed = "fixed"

instance FromForm ShareType where
  fromForm form =
    parseUnique "share-type" form >>= \case
      ("percentage" :: Text) -> Right Percentage
      ("fixed" :: Text) -> Right Fixed
      _ -> Left "Invalid share type"

data Split = Split {shares :: [Share]}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data IOU = IOU {from :: Person, to :: Person, amount :: Amount}
  deriving (Show)

iou :: Transaction -> [IOU]
iou (ExpenseTransaction (Expense {paidBy, total, split})) =
  [IOU s.person paidBy (calcDebt total s) | s <- split.shares, s.person /= paidBy]
iou (SettlementTransaction (Settlement {from, to, amount})) = [IOU to from amount]

settlements :: TimeZone -> UTCTime -> [Transaction] -> [Settlement]
settlements tz utc ts = [Settlement p creditor amount (utcToLocalTime tz utc) | (p, debts) <- debtsToList (simplifiedDebts ts), (creditor, amount) <- debts]

formatDate :: LocalTime -> Text
formatDate = T.pack . formatTime defaultTimeLocale "%F - %R"

calcDebt :: Amount -> Share -> Amount
calcDebt total (Share Percentage _ p _) = (p * total) / 100
calcDebt _ (Share Fixed _ a _) = a

iousToMap :: [IOU] -> IOUMap
iousToMap = IOUMap . foldl' (\m i -> M.insertWith (M.unionWith (+)) i.from (M.singleton i.to i.amount) m) M.empty

ious :: [Transaction] -> [IOU]
ious = concatMap iou

mkSplit :: Amount -> [Share] -> Either Text Split
mkSplit total shares
  | any ((< 0) . shareAmount) shares = Left "Shares may not be negative"
  | sum (map (calcDebt total) shares) /= total = Left "Sum of shares does not equal total"
  | length (nub shares) /= length shares = Left "A person may not be listed more than once in the split"
  | length (shares) < 2 = Left "An expense must be split between at least two people"
  | otherwise = Right $ Split $ sortOn (.person) shares

tally :: IOUMap -> M.Map Person Amount
tally = M.foldlWithKey' mkPersonalTally M.empty . coerce
  where
    mkPersonalTally :: M.Map Person Amount -> Person -> M.Map Person Amount -> M.Map Person Amount
    mkPersonalTally m p debts =
      let negative = M.singleton p (negate $ sum $ M.elems debts) -- subtract the amount of my debts from my tally
       in M.unionsWith (+) [debts, negative, m] -- add 'debts' to the union, to add credits to lenders

splitTally :: M.Map Person Amount -> ([Tally Owed], [Tally Owing])
splitTally =
  bimap (map (uncurry Tally)) (map (uncurry Tally . second negate))
    . partition ((>= 0) . snd)
    . M.toList

data Tally a = Tally Person Amount
  deriving (Show)

data Owing

data Owed

newtype IOUMap = IOUMap (M.Map Person (M.Map Person Amount))

tallies :: [Transaction] -> ([Tally Owed], [Tally Owing])
tallies = splitTally . tally . iousToMap . ious

peopleOfExpense :: Expense -> [Person]
peopleOfExpense (Expense {split}) = map person split.shares

singleDebtor :: Expense -> Maybe Share
singleDebtor exp = find entered exp.split.shares

shareAmount :: Share -> Amount
shareAmount = (.amount)

findExpense :: UUID -> [Transaction] -> Maybe Expense
findExpense i = find isExpense >=> getExpense
  where
    isExpense (ExpenseTransaction e) = i == e.id
    isExpense _ = False

    getExpense (ExpenseTransaction e) = Just e
    getExpense _ = Nothing
