module Splitvajs where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard)
import Data.List (foldl', nub, sortOn)
import Data.Map qualified as M
import Data.Map.Strict qualified as SM
import Data.Maybe (fromJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Lucid
import Prelude hiding (exp, product)

data Person = Person {name :: Text, color :: Text}
  deriving (Eq, Ord)

instance Show Person where
  show = T.unpack . name

instance ToHtml Person where
  toHtml person = toHtml $ person.name
  toHtmlRaw = toHtml

ola :: Person
ola = Person "Ola" "RoyalBlue"

wilma :: Person
wilma = Person "Wilma" "DarkRed"

exampleExpenses :: [Expense]
exampleExpenses = take 10 $ cycle [exampleSplit ola 70 30 100, exampleSplit ola 50 50 100, exampleSplit wilma 40 60 100]

-- >>> ious exampleExpenses
-- fromList [(Ola,fromList [(Wilma,50.0)])]

exampleSplit :: Person -> Int -> Int -> Double -> Expense
exampleSplit p sh1 sh2 total =
  Expense
    { split = fromJust $ mkSplit [(Share ola sh1), (Share wilma sh2)],
      total = total,
      paidBy = p,
      title = "IKEA bord",
      date = read "2021-09-01 12:00:00"
    }

type Amount = Double

data Expense = Expense
  { split :: Split,
    total :: Amount,
    paidBy :: Person,
    title :: Text,
    date :: UTCTime
  }

-- TODO: absolute shares
data Share = Share
  { person :: Person,
    share :: Int
  }
  deriving (Eq)

data Split = Split {shares :: [Share]}

data Debtor = Debtor {person :: Person, amount :: Amount}
  deriving (Show)

-- Given an expense, this is how much a person is owed
-- from the other people in the split
iou :: Expense -> (Person, [Debtor])
iou exp = (exp.paidBy, debtors)
  where
    calcDebt :: Amount -> Share -> Amount
    calcDebt total = (* total) . (/ 100) . fromIntegral . share
    debtors =
      map (Debtor <$> (.person) <*> calcDebt exp.total) $
        filter ((/= exp.paidBy) . (.person)) $
          exp.split.shares

ious :: [Expense] -> M.Map Person (M.Map Person Amount)
ious = foldl' addDebt mempty . map iou
  where
    addDebt :: M.Map Person (M.Map Person Amount) -> (Person, [Debtor]) -> M.Map Person (M.Map Person Amount)
    addDebt m (p, debtors) = SM.insertWith (M.unionWith (+)) p (collectDebts debtors) m

    collectDebts :: [Debtor] -> M.Map Person Amount
    collectDebts = M.fromListWith (+) . map ((.person) &&& amount)

mkSplit :: [Share] -> Maybe Split
mkSplit shares = do
  guard $ sum (map share shares) == 100
  guard $ length (nub shares) == length shares
  Just $ Split $ sortOn (Down . (.person)) shares
