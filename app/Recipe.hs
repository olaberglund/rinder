module Recipe where

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (fold)
import Data.Set (Set, intersection, toList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lucid (ToHtml)
import Lucid.Base (ToHtml (toHtml, toHtmlRaw))
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)
import Willys (Promotion, SuperProduct, image, imageUrls, product)
import Prelude hiding (product)

data Recipe = Recipe
  { name :: !Text,
    ingredients :: !(Set SuperProduct)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RecipeForm = RecipeForm
  { unvalidatedName :: !Text,
    unvalidatedIngredients :: !(Set Text)
  }
  deriving (Show, Eq, Ord, Generic)

instance FromForm RecipeForm where
  fromForm form =
    RecipeForm
      <$> parseUnique "name" form
      <*> (parseIngredients <$> parseUnique "ingredients" form)

instance ToHtml Recipe where
  toHtml (Recipe name _ingredients) = toHtml name

  toHtmlRaw = toHtml

parseIngredients :: Text -> Set Text
parseIngredients = Set.fromList . Text.splitOn "\r\n"

-- | Given a set of recipes and promotions,
-- | calculate a list of recipes where
-- | at least n ingredients are on promotion
recipeSuggestions :: Set Recipe -> Set Promotion -> Int -> Set Recipe
recipeSuggestions recipes promotions n =
  let urlsOfRecipe = fold . Set.map imageUrls . ingredients
      promotionUrls = Set.map (image . product) promotions
      nCommonIngredients = (>= n) . Set.size . intersection promotionUrls . urlsOfRecipe
   in Set.filter nCommonIngredients recipes
