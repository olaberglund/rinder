module Recipe where

import Data.Set (Set, intersection, size, toList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lucid (ToHtml, h1_)
import Lucid.Base (ToHtml (toHtml, toHtmlRaw))
import Lucid.Html5
import Web.FormUrlEncoded (FromForm (fromForm), parseUnique)
import Willys (Product (..), Promotion)
import Willys qualified

data Recipe = Recipe
  { name :: !Text,
    ingredients :: !(Set Product)
  }
  deriving (Show, Eq, Ord, Generic)

instance FromForm Recipe where
  fromForm form =
    Recipe
      <$> parseUnique "name" form
      <*> (parseIngredients <$> parseUnique "ingredients" form)

instance ToHtml Recipe where
  toHtml (Recipe name ingredients) = do
    h2_ (toHtml name)
    mapM_ (span_ . toHtml) (toList ingredients)

  toHtmlRaw = toHtml

parseIngredients :: Text -> Set Product
parseIngredients = Set.fromList . map Product . Text.lines

-- | Given a set of recipes and promotions,
-- | calculate a list of recipes where
-- | at least n ingredients are on promotion
recipeSuggestions :: Set Recipe -> Set Promotion -> Int -> Set Recipe
recipeSuggestions recipes promotions n =
  Set.filter
    ((>= n) . size . (`intersection` Set.map Willys.product promotions) . ingredients)
    recipes
