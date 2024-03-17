module Recipe where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set, intersection)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid (ToHtml)
import Lucid.Base (ToHtml (toHtml, toHtmlRaw))
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)
import Willys (Product, Promotion, image, product)
import Prelude hiding (product)

data Recipe = Recipe
  { name :: !Text,
    ingredients :: ![Product]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromForm Recipe where
  fromForm form = do
    nm <- parseUnique "name" form
    products :: [Product] <- fromForm form
    return $ Recipe nm products

instance ToHtml Recipe where
  toHtml (Recipe name _ingredients) = toHtml name

  toHtmlRaw = toHtml

-- | Given a set of recipes and promotions,
-- | calculate a list of recipes where
-- | at least n ingredients are on promotion
recipeSuggestions :: Set Recipe -> Set Promotion -> Int -> Set Recipe
recipeSuggestions recipes promotions n =
  let urlsOfRecipe = map image . ingredients
      promotionUrls = Set.map (image . product) promotions
      nCommonIngredients = (>= n) . Set.size . intersection promotionUrls . Set.fromList . urlsOfRecipe
   in Set.filter nCommonIngredients recipes
