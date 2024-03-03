module Recipe where

import Data.Set (Set, intersection, size)
import Data.Set qualified as Set
import Data.Text (Text)
import Willys (Product (..), Promotion)
import Willys qualified

data Recipe = Recipe
  { name :: !Text,
    ingredients :: !(Set Product),
    steps :: ![Text]
  }
  deriving (Show, Eq, Ord)

-- | Given a set of recipes and promotions,
-- | calculate a list of recipes where
-- | at least n ingredients are on promotion
recipeSuggestions :: Set Recipe -> Set Promotion -> Int -> Set Recipe
recipeSuggestions recipes promotions n =
  Set.filter
    ((>= n) . size . (`intersection` Set.map Willys.product promotions) . ingredients)
    recipes
