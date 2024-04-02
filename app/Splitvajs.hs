module Splitvajs (
    Amount,
    Expense (..),
    ExpenseForm,
    Person (..),
    Settlement (..),
    Share (..),
    ShareType (..),
    Split (..),
    Transaction (..),
    findExpense,
    formatDate,
    people,
    peopleOfExpense,
    settlements,
    simplifiedDebts,
    singleDebtor,
    toExpense,
    shareTypeSymbol,
    shareTypeToText,
) where

import Control.Arrow qualified as Arrow
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Lucid
import Safe qualified as Safe
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

data Person = Person
    { personName :: !Text
    , personColor :: !Text
    }
    deriving stock (Generic, Ord, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

instance ToHtml Person where
    toHtml = toHtml . personName
    toHtmlRaw = toHtml

people :: [Person]
people = [wilma, ola]

ola :: Person
ola = Person "Ola" "RoyalBlue"

wilma :: Person
wilma = Person "Wilma" "DarkRed"

{- | Pay off debts
| Takes a list of people with positive credit and a debt.
| Returns a list of IOUs and a list of people with remaining credit.
-}
payOff :: [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
payOff credits debts = payOff' [] credits debts
  where
    payOff' :: [IOU] -> [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
    payOff' ds (Tally p credit : ts) (Tally p' debt) =
        case compare credit debt of
            GT -> (IOU p' p debt : ds, Tally p (credit - debt) : ts)
            EQ -> (IOU p' p credit : ds, ts)
            LT -> payOff' (IOU p' p credit : ds) ts (Tally p' (debt - credit))
    payOff' _ [] _ = ([], [])

simplifiedDebts :: [Transaction] -> [(Person, [(Person, Amount)])]
simplifiedDebts = debtsToList . minimizeTransactions . tallies
  where
    debtsToList :: IouMap -> [(Person, [(Person, Amount)])]
    debtsToList = Map.toList . Map.map Map.toList . coerce

minimizeTransactions :: ([Tally Owed], [Tally Owing]) -> IouMap
minimizeTransactions =
    IouMap . Map.unions @[] . coerce . List.unfoldr payOffStep
  where
    payOffStep ::
        ([Tally Owed], [Tally Owing]) ->
        Maybe (IouMap, ([Tally Owed], [Tally Owing]))
    payOffStep (cs, d : ds) =
        let (ious', cs') = payOff cs d
         in Just (iousToMap ious', (cs', ds))
    payOffStep _ = Nothing

type Amount = Float

data Transaction
    = ExpenseTransaction !Expense
    | SettlementTransaction !Settlement
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data Expense = Expense
    { expenseSplit :: !Split
    , expenseTotal :: !Amount
    , expensePaidBy :: !Person
    , expenseRubric :: !Text
    , expenseId :: !UUID
    , expenseDate :: !Time.LocalTime
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Eq Expense where
    (==) = (==) `on` expenseId

data Settlement = Settlement
    { settlementFrom :: !Person
    , settlementTo :: !Person
    , settlementAmount :: !Amount
    , settlementDate :: !Time.LocalTime
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

-- | the current implementation of the expense form
data ExpenseForm = ExpenseForm
    { efDebtor :: !Person
    , efPaidBy :: !Person
    , efSplit :: !Split
    , efShareType :: !ShareType
    , efAmount :: !Float
    , efRubric :: !Text
    , efTotal :: !Amount
    }
    deriving stock (Show, Eq)

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
                    else
                        mkSplit
                            total
                            [ Share Percentage debtor amount True
                            , Share Percentage oPerson (100 - amount) False
                            ]
            Fixed ->
                mkSplit
                    total
                    [ Share Fixed debtor amount True
                    , Share Fixed oPerson (total - amount) False
                    ]
        pure $ ExpenseForm debtor paidBy split' shareType amount rubric total
      where
        findPerson :: Text -> Either Text Person
        findPerson n =
            maybe (Left $ "Person " <> n <> " not found") Right $
                List.find ((== n) . personName) people

        otherPerson :: Person -> Maybe Person
        otherPerson p = Safe.headMay (filter (/= p) people)

        mayNotBeNegative :: Text -> Float -> Either Text Float
        mayNotBeNegative prop a
            | a >= 0 = Right a
            | otherwise = Left $ prop <> " may not be negative"

        mayNotBeEmpty :: Text -> Either Text Text
        mayNotBeEmpty rubric
            | Text.null rubric = Left "Rubric may not be empty"
            | otherwise = Right rubric

        note :: Text -> Maybe a -> Either Text a
        note e = maybe (Left e) Right

toExpense :: ExpenseForm -> UUID -> Time.LocalTime -> Expense
toExpense (ExpenseForm{efSplit, efTotal, efPaidBy, efRubric}) =
    Expense efSplit efTotal efPaidBy efRubric

data ShareType
    = Percentage
    | Fixed
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

data Share = Share
    { shareType :: !ShareType
    , sharePerson :: !Person
    , shareAmount :: !Float
    , shareEntered :: !Bool
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

shareTypeToText :: ShareType -> Text
shareTypeToText Percentage = "percentage"
shareTypeToText Fixed = "fixed"

shareTypeSymbol :: ShareType -> Text
shareTypeSymbol Percentage = "%"
shareTypeSymbol Fixed = "kr"

instance FromForm ShareType where
    fromForm form =
        parseUnique "share-type" form >>= \case
            ("percentage" :: Text) -> Right Percentage
            ("fixed" :: Text) -> Right Fixed
            _ -> Left "Invalid share type"

data Split = Split
    { splitShares :: ![Share]
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data IOU = IOU
    { iouFrom :: !Person
    , iouTo :: !Person
    , iouAmount :: !Amount
    }
    deriving stock (Show, Eq)

iou :: Transaction -> [IOU]
iou (ExpenseTransaction e) =
    [ IOU (sharePerson s) (expensePaidBy e) (calcDebt (expenseTotal e) s)
    | s <- splitShares (expenseSplit e)
    , sharePerson s /= expensePaidBy e
    ]
iou (SettlementTransaction s) =
    [IOU (settlementTo s) (settlementFrom s) (settlementAmount s)]

settlements :: Time.TimeZone -> Time.UTCTime -> [Transaction] -> [Settlement]
settlements tz utc ts =
    [ Settlement p creditor amount (Time.utcToLocalTime tz utc)
    | (p, debts) <- simplifiedDebts ts
    , (creditor, amount) <- debts
    ]

formatDate :: Time.LocalTime -> Text
formatDate = Text.pack . Time.formatTime Time.defaultTimeLocale "%F - %R"

calcDebt :: Amount -> Share -> Amount
calcDebt total (Share Percentage _ p _) = (p * total) / 100
calcDebt _ (Share Fixed _ a _) = a

iousToMap :: [IOU] -> IouMap
iousToMap =
    IouMap
        . List.foldl'
            ( \m i ->
                Map.insertWith
                    (Map.unionWith (+))
                    (iouFrom i)
                    (newDebt i)
                    m
            )
            Map.empty
  where
    newDebt :: IOU -> Map.Map Person Amount
    newDebt = Map.singleton <$> iouTo <*> iouAmount

ious :: [Transaction] -> [IOU]
ious = concatMap iou

mkSplit :: Amount -> [Share] -> Either Text Split
mkSplit total shares
    | any ((< 0) . shareAmount) shares = Left "Shares may not be negative"
    | sum (map (calcDebt total) shares) /= total =
        Left "Sum of shares does not equal total"
    | length (List.nub shares) /= length shares =
        Left "A person may not be listed more than once in the split"
    | length (shares) < 2 =
        Left "An expense must be split between at least two people"
    | otherwise = Right $ Split $ List.sortOn sharePerson shares

tally :: IouMap -> Map.Map Person Amount
tally = Map.foldlWithKey' mkPersonalTally Map.empty . coerce
  where
    mkPersonalTally ::
        Map.Map Person Amount ->
        Person ->
        Map.Map Person Amount ->
        Map.Map Person Amount
    mkPersonalTally m p debts =
        let negative = Map.singleton p (negate $ sum $ Map.elems debts) -- subtract the amount of my debts from my tally
         in Map.unionsWith (+) [debts, negative, m] -- add 'debts' to the union, to add credits to lenders

splitTally :: Map.Map Person Amount -> ([Tally Owed], [Tally Owing])
splitTally =
    bimap (map (uncurry Tally)) (map (uncurry Tally . Arrow.second negate))
        . List.partition ((>= 0) . snd)
        . Map.toList

data Tally a = Tally !Person !Amount
    deriving stock (Show, Eq)

data Owing

data Owed

newtype IouMap = IouMap
    { unIouMap :: Map.Map Person (Map.Map Person Amount)
    }
    deriving stock (Show)
    deriving newtype (Eq)

tallies :: [Transaction] -> ([Tally Owed], [Tally Owing])
tallies = splitTally . tally . iousToMap . ious

peopleOfExpense :: Expense -> [Person]
peopleOfExpense = map sharePerson . splitShares . expenseSplit

singleDebtor :: Expense -> Maybe Share
singleDebtor = List.find shareEntered . splitShares . expenseSplit

findExpense :: UUID -> [Transaction] -> Maybe Expense
findExpense i = List.find isExpense >=> getExpense
  where
    isExpense :: Transaction -> Bool
    isExpense (ExpenseTransaction e) = i == expenseId e
    isExpense _ = False

    getExpense :: Transaction -> Maybe Expense
    getExpense (ExpenseTransaction e) = Just e
    getExpense _ = Nothing
