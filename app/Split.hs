{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Split (
    Amount,
    showShare,
    Expense (..),
    ExpenseForm,
    Person (..),
    Settlement (..),
    Share (..),
    showAmount,
    ShareType (..),
    Value (..),
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
) where

import qualified Control.Arrow      as Arrow
import           Control.Monad      ((>=>))
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Bifunctor     (bimap)
import           Data.Coerce        (coerce)
import           Data.Foldable      (foldl')
import           Data.Function      (on)
import qualified Data.List          as List
import qualified Data.Map           as Map
import           Data.Ratio         (approxRational)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Time          as Time
import           Data.UUID          (UUID)
import           Debug.Trace
import           GHC.Generics       (Generic)
import qualified Money
import           Money.Aeson        ()
import qualified Safe
import           Web.FormUrlEncoded (FromForm (fromForm), parseUnique)

data Person = Person
    { personName  :: !Text
    , personColor :: !Text
    }
    deriving stock (Generic, Ord, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | A list of people who are part of the app
people :: [Person]
people = [wilma, ola]

ola :: Person
ola = Person "Ola" "RoyalBlue"

wilma :: Person
wilma = Person "Wilma" "DarkRed"

{- | Calculates the IOUs and the remaining debts after paying off
the debts of a person.

The algorithm compares the person's debt against the
credit of another person:

If the debt is greater than the credit, the
debt is reduced by the credit and an IOU from the debtor to the creditor
is created.

If the debt is equal to the credit, the debt is paid off and an IOU is
created. The the creditor is also removed from the list of creditors.

If the debt is less than the credit, the debt is paid off and an IOU is
created. The credit is reduced by the debt amount.
-}
payOff :: [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
payOff = payOff' []
  where
    payOff' :: [IOU] -> [Tally Owed] -> Tally Owing -> ([IOU], [Tally Owed])
    payOff' ds (Tally p credit : ts) (Tally p' debt) =
        case compare credit debt of
            GT -> (IOU p' p debt : ds, Tally p (credit - debt) : ts)
            EQ -> (IOU p' p credit : ds, ts)
            LT -> payOff' (IOU p' p credit : ds) ts (Tally p' (debt - credit))
    payOff' _ [] _ = ([], [])

{- |  Calculates a minimized list of debts between people, given a list of
transactions.

First, the transactions are split into two lists of tallies, one for
people who owe money and one for people who are owed money.

Then, debts (IOUs) are created by successively paying off the debts of the
people who owe money to the people who are owed money.
-}
simplifiedDebts :: [Transaction] -> [(Person, [(Person, Amount)])]
simplifiedDebts = debtsToList . debts . tallies
  where
    debtsToList :: DebtMap -> [(Person, [(Person, Amount)])]
    debtsToList = Map.toList . Map.map Map.toList . coerce

    tallies :: [Transaction] -> ([Tally Owed], [Tally Owing])
    tallies = splitTally . tally . iousToMap . concatMap iou

    splitTally :: Map.Map Person Amount -> ([Tally Owed], [Tally Owing])
    splitTally =
        bimap (map (uncurry Tally)) (map (uncurry Tally . Arrow.second negate))
            . List.partition ((>= 0) . snd)
            . Map.toList

-- | Calculates the debts from a tally of creditors and debtors
debts :: ([Tally Owed], [Tally Owing]) -> DebtMap
debts = Map.unions . List.unfoldr payOffStep
  where
    payOffStep ::
        ([Tally Owed], [Tally Owing]) ->
        Maybe (DebtMap, ([Tally Owed], [Tally Owing]))
    payOffStep (cs, d : ds) =
        let (ious', cs') = payOff cs d
         in Just (iousToMap ious', (cs', ds))
    payOffStep _ = Nothing

type Amount = Money.Dense "SEK"

data Transaction
    = ExpenseTransaction !Expense
    | SettlementTransaction !Settlement
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data Expense = Expense
    { expenseSplit  :: !Split
    , expensePaidBy :: !Person
    , expenseRubric :: !Text
    , expenseTotal  :: !Amount
    , expenseId     :: !UUID
    , expenseDate   :: !Time.LocalTime
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Eq Expense where
    (==) = (==) `on` expenseId

{- | A settlement is a transaction where one person pays another person
an amount of money to presumably settle a debt.
-}
data Settlement = Settlement
    { settlementFrom   :: !Person
    , settlementTo     :: !Person
    , settlementAmount :: !Amount
    , settlementDate   :: !Time.LocalTime
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

-- | the current implementation of the expense form
data ExpenseForm = ExpenseForm
    { efDebtor :: !Person
    , efPaidBy :: !Person
    , efSplit  :: !Split
    , efRubric :: !Text
    , efTotal  :: !Amount
    }
    deriving stock (Show, Eq)

-- instance FromForm ShareValue where
--     fromForm form =
--         parseUnique "share-type" form >>= \case
--             ("percentage" :: Text) -> Right Percentage
--             ("fixed" :: Text) -> Right Fixed
--             _ -> Left "Invalid share type"

instance FromForm ExpenseForm where
    fromForm form = do
        efDebtor <- parseUnique "debtor" form >>= findPerson
        efPaidBy <- parseUnique "paidBy" form >>= findPerson
        efRubric <- parseUnique "rubric" form >>= mayNotBeEmpty
        oPerson <- note "Couldn't find person" (otherPerson efDebtor)
        shareType :: Text <- parseUnique "share-type" form
        efTotal <- parseUnique "total" form >>= parseMoney
        efSplit <- case shareType of
            "percentage" -> do
                pct <- parseUnique "value" form >>= mayNotBeNegative "Percentage"
                let pct' = approxRational pct 0.0001 / 100
                if pct' > 1
                    then Left "Percentage may not be greater than 100"
                    else mkSplit efTotal [Share (Value Percentage pct') efDebtor True, Share (Value Percentage (1 - pct')) oPerson False]
            "fixed" -> do
                amount <- parseUnique "value" form >>= mayNotBeNegative "Amount"
                let amount' = approxRational amount 0.0001
                mkSplit
                    efTotal
                    [ Share (Value Fixed amount') efDebtor True
                    , Share (Value Fixed (toRational efTotal - amount')) oPerson False
                    ]
            _ -> Left "Invalid share type"
        pure $ ExpenseForm{..}
      where
        findPerson :: Text -> Either Text Person
        findPerson n =
            maybe (Left $ "Person " <> n <> " not found") Right $
                List.find ((== n) . personName) people

        parseMoney :: Text -> Either Text Amount
        parseMoney = note "Could not parse money" . Money.denseFromDecimal Money.defaultDecimalConf

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

-- | An expense is dated, and has a unique identifier
toExpense :: ExpenseForm -> UUID -> Time.LocalTime -> Expense
toExpense (ExpenseForm{efSplit, efTotal, efPaidBy, efRubric}) =
    Expense efSplit efPaidBy efRubric efTotal

-- | A person can be responsible for a percentage or fixed amount of an expense
data ShareType = Percentage | Fixed
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

data Value = Value
    { valueType :: ShareType
    , value     :: Rational
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

data Share = Share
    { shareValue   :: !Value
    , sharePerson  :: !Person
    , shareEntered :: !Bool
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

newtype Split = Split
    { splitShares :: [Share]
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data IOU = IOU
    { iouFrom   :: !Person
    , iouTo     :: !Person
    , iouAmount :: !Amount
    }
    deriving stock (Show, Eq)

iou :: Transaction -> [IOU]
iou (ExpenseTransaction (Expense{..})) =
    [ IOU sharePerson expensePaidBy (share expenseTotal shareValue)
    | Share{..} <- splitShares expenseSplit
    , sharePerson /= expensePaidBy
    ]
iou (SettlementTransaction (Settlement{..})) =
    [IOU settlementTo settlementFrom settlementAmount]

settlements :: Time.TimeZone -> Time.UTCTime -> [Transaction] -> [Settlement]
settlements tz utc ts =
    [ Settlement p creditor amount (Time.utcToLocalTime tz utc)
    | (p, d) <- simplifiedDebts ts
    , (creditor, amount) <- d
    ]

formatDate :: Time.LocalTime -> Text
formatDate = Text.pack . Time.formatTime Time.defaultTimeLocale "%F - %R"

share :: Amount -> Value -> Amount
share total (Value st v) = case st of
    Percentage -> total * Money.dense' v
    Fixed      -> Money.dense' v

iousToMap :: [IOU] -> DebtMap
iousToMap = List.foldl' accumDebts Map.empty
  where
    newDebt :: IOU -> Map.Map Person Amount
    newDebt = Map.singleton <$> iouTo <*> iouAmount

    accumDebts ::
        Map.Map Person (Map.Map Person Amount) ->
        IOU ->
        Map.Map Person (Map.Map Person Amount)
    accumDebts m i =
        Map.insertWith
            (Map.unionWith (+))
            (iouFrom i)
            (newDebt i)
            m

mkSplit :: Amount -> [Share] -> Either Text Split
mkSplit total shares
    | toRational total == foldl' (+) 0 (value . shareValue <$> shares) =
        Left "Sum of shares does not equal total"
    | length (List.nub shares) /= length shares =
        Left "A person may not be listed more than once in the split"
    | length shares < 2 =
        Left "An expense must be split between at least two people"
    | otherwise = Right $ Split $ List.sortOn sharePerson shares

tally :: DebtMap -> Map.Map Person Amount
tally = Map.foldlWithKey' mkPersonalTally Map.empty
  where
    mkPersonalTally ::
        -- the map of debts/credits
        Map.Map Person Amount ->
        -- the person whose debts/credits are being tallied (key)
        Person ->
        -- the debts/credits of the person in regards to other people (value)
        Map.Map Person Amount ->
        -- the updated map of debts/credits
        Map.Map Person Amount
    mkPersonalTally m p ds =
        --  subtract the amount of my debts from my tally
        let negative = Map.singleton p (negate $ sum $ Map.elems ds)
         in -- add 'debts' to the union, to add credits to lenders
            Map.unionsWith (+) [ds, negative, m]

{- | A person's debt or credit (both are positive).
The parameter is a phantom type to distinguish between debts and credits.
-}
data Tally a = Tally !Person !Amount
    deriving stock (Show, Eq)

data Owing

data Owed

-- | A map of people and their debts to other people
type DebtMap = Map.Map Person (Map.Map Person Amount)

peopleOfExpense :: Expense -> [Person]
peopleOfExpense = map sharePerson . splitShares . expenseSplit

{- | Find the person that was entered as the debtor in the expense.
It is named 'singleDebtor' because an expense can only have one debtor as
of now.
-}
singleDebtor :: Expense -> Maybe Share
singleDebtor = List.find shareEntered . splitShares . expenseSplit

findExpense :: UUID -> [Transaction] -> Maybe Expense
findExpense i = List.find isExpense >=> getExpense
  where
    isExpense :: Transaction -> Bool
    isExpense (ExpenseTransaction e) = i == expenseId e
    isExpense _                      = False

    getExpense :: Transaction -> Maybe Expense
    getExpense (ExpenseTransaction e) = Just e
    getExpense _                      = Nothing

showAmount :: Amount -> Text
showAmount = Money.denseToDecimal Money.defaultDecimalConf Money.Round

showShare :: Share -> Text
showShare = showAmount . Money.dense' . value . shareValue
