module Splitvajs (ola, ylva, wilma, Person (..), exampleExpenses, Expense (..), Split (..), Amount, Share (..), ious, transactions, IOUMap (..)) where

import Control.Arrow (second)
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.List (foldl', nub, partition, sortOn, unfoldr)
import Data.Map qualified as M
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

ylva :: Person
ylva = Person "Ylva" "DarkGreen"

exampleExpenses :: [Expense]
exampleExpenses = [exampleSplit ola 20 60 20 100, exampleSplit wilma 30 20 50 100]

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

transactions :: [Expense] -> IOUMap
transactions = uncurry simplifyTransactions . tallies

simplifyTransactions :: [Tally Owed] -> [Tally Owing] -> IOUMap
simplifyTransactions credits debts = IOUMap $ M.unions @[] $ coerce $ unfoldr payOffStep (credits, debts)
  where
    payOffStep :: ([Tally Owed], [Tally Owing]) -> Maybe (IOUMap, ([Tally Owed], [Tally Owing]))
    payOffStep (cs, d : ds) = let (ious', cs') = payOff cs d in Just (iousToMap ious', (cs', ds))
    payOffStep _ = Nothing

exampleSplit :: Person -> Float -> Float -> Float -> Amount -> Expense
exampleSplit p sh1 sh2 sh3 total =
  Expense
    { split = fromJust $ mkSplit total [(Percentage ola sh1), (Percentage ylva sh2), (Percentage wilma sh3)],
      total = total,
      paidBy = p,
      title = "Matvaror",
      date = read "2021-09-01 12:00:00"
    }

type Amount = Float

data Expense = Expense
  { split :: Split,
    total :: Amount,
    paidBy :: Person,
    title :: Text,
    date :: UTCTime
  }

data Share = Percentage {person :: Person, percent :: Float} | Fixed {person :: Person, amount :: Amount}
  deriving (Eq)

data Split = Split {shares :: [Share]}

data IOU = IOU {from :: Person, to :: Person, amount :: Amount}
  deriving (Show)

iou :: Expense -> [IOU]
iou exp = [IOU s.person exp.paidBy (calcDebt exp.total s) | s <- exp.split.shares, s.person /= exp.paidBy]

calcDebt :: Amount -> Share -> Amount
calcDebt total (Percentage _ p) = (p * total) / 100
calcDebt _ (Fixed _ a) = a

iousToMap :: [IOU] -> IOUMap
iousToMap = IOUMap . foldl' (\m i -> M.insertWith (M.unionWith (+)) i.from (M.singleton i.to i.amount) m) M.empty

ious :: [Expense] -> [IOU]
ious = concatMap iou

mkSplit :: Amount -> [Share] -> Maybe Split
mkSplit total shares = do
  guard $ sum (map (calcDebt total) shares) == total
  guard $ length (nub shares) == length shares
  Just $ Split $ sortOn (Down . (calcDebt total)) shares

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
    . partition ((> 0) . snd)
    . filter ((/= 0) . snd)
    . M.toList

data Tally a = Tally Person Amount
  deriving (Show)

data Owing

data Owed

newtype IOUMap = IOUMap (M.Map Person (M.Map Person Amount))

tallies :: [Expense] -> ([Tally Owed], [Tally Owing])
tallies = splitTally . tally . iousToMap . ious
