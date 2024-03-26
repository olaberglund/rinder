module Splitvajs where

import Control.Monad (guard)
import Data.List (foldl', nub, sortOn)
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
exampleExpenses = take 20 $ cycle [exampleSplit ola 70 30 100, exampleSplit' ylva 10 10 80 100, exampleSplit' wilma 25 35 40 100, exampleSplit ola 50 50 100, exampleSplit wilma 40 60 80]

exampleSplit :: Person -> Int -> Int -> Amount -> Expense
exampleSplit p sh1 sh2 total =
  Expense
    { split = fromJust $ mkSplit [(Share ola sh1), (Share wilma sh2)],
      total = total,
      paidBy = p,
      title = "Matvaror",
      date = read "2021-09-01 12:00:00"
    }

exampleSplit' :: Person -> Int -> Int -> Int -> Amount -> Expense
exampleSplit' p sh1 sh2 sh3 total =
  Expense
    { split = fromJust $ mkSplit [(Share ola sh1), (Share ylva sh2), (Share wilma sh3)],
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

data Share = Share {person :: Person, share :: Int}
  deriving (Eq)

data Split = Split {shares :: [Share]}

data IOU = IOU {from :: Person, to :: Person, amount :: Amount}
  deriving (Show)

iou :: Expense -> [IOU]
iou exp = [IOU s.person exp.paidBy (calcDebt exp.total s) | s <- exp.split.shares, s.person /= exp.paidBy]
  where
    calcDebt :: Amount -> Share -> Amount
    calcDebt total = (* total) . (/ 100) . fromIntegral . share

ious :: [Expense] -> M.Map Person (M.Map Person Amount)
ious = foldl' (\m i -> M.insertWith (M.unionWith (+)) i.from (M.singleton i.to i.amount) m) M.empty . concatMap iou

mkSplit :: [Share] -> Maybe Split
mkSplit shares = do
  guard $ sum (map share shares) == 100
  guard $ length (nub shares) == length shares
  Just $ Split $ sortOn (Down . (.share)) shares
