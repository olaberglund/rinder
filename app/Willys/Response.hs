module Willys.Response (
    ProductResponse,
    PromotionResponse,
    Promotion (..),
    Product (..),
    ImageUrl (..),
    responseResults,
    StripAndLower,
    getCartLabel,
    getPrice,
    getSavePrice,
    getId,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord qualified as Ord
import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson (CustomJSON, FieldLabelModifier, Rename, StripPrefix)
import Deriving.Aeson qualified
import GHC.Generics (Generic)
import Lucid (ToHtml (..))
import Safe qualified

data Response a = Response
    { responseResults :: ![a]
    , responsePagination :: !Pagination
    }
    deriving stock (Generic, Show)
    deriving (FromJSON, ToJSON) via StripAndLower "response" (Response a)

type StripAndLower a b =
    CustomJSON
        '[FieldLabelModifier '[StripPrefix a, PascalToCamel]]
        b

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

data PascalToCamel

instance Deriving.Aeson.StringModifier PascalToCamel where
    getStringModifier = lowerCaseFirst
      where
        lowerCaseFirst :: String -> String
        lowerCaseFirst (c : cs) = Char.toLower c : cs
        lowerCaseFirst s = s

newtype Pagination = Pagination {unPagination :: Int}
    deriving stock (Generic, Show)
    deriving newtype (Num)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[FieldLabelModifier '[Rename "unPagination" "numberOfPages"]]
                Pagination

{- Promotion -}

newtype Promotion = Promotion {unPromotion :: Product}
    deriving stock (Generic, Show)
    deriving newtype (Ord, Eq)

instance Aeson.FromJSON Promotion where
    parseJSON = Aeson.withObject "Promotion" $ \v -> do
        productName :: Text <- v .: "name"
        imageUrl :: Text <- v .: "image" >>= (.: "url")
        price :: Maybe Text <- v .: "price"
        potentialPromotions :: [PotentialPromotion] <- v .: "potentialPromotions"
        return $
            Promotion
                ( Product
                    productName
                    (ImageUrl imageUrl)
                    price
                    potentialPromotions
                )

data Product = Product
    { productName :: !Text
    , productImage :: !ImageUrl
    , productPrice :: !(Maybe Text)
    , productPotentialPromotions :: ![PotentialPromotion]
    }
    deriving stock (Generic, Show)
    deriving (FromJSON, ToJSON) via StripAndLower "product" Product

instance Eq Product where
    (==) = (==) `on` getId

instance Ord Product where
    compare = Ord.comparing productImage

instance ToHtml Product where
    toHtml = toHtml . productName
    toHtmlRaw = toHtml

data PotentialPromotion = PotentialPromotion
    { ppCartLabel :: !(Maybe Text)
    , ppSavePrice :: !(Maybe Text)
    }
    deriving stock (Generic, Show, Eq, Ord)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[FieldLabelModifier '[StripPrefix "pp", PascalToCamel]]
                PotentialPromotion

newtype ImageUrl = ImageUrl
    { unImageUrl :: Text
    }
    deriving stock (Generic, Show)
    deriving newtype (Eq, Ord)
    deriving (FromJSON, ToJSON) via StripAndLower "unImage" ImageUrl

getId :: Product -> Text
getId p =
    Text.filter (/= ' ') (productName p)
        <> Text.filter Char.isNumber (unImageUrl (productImage p))

getCartLabel :: Product -> Maybe Text
getCartLabel p = Safe.headMay (productPotentialPromotions p) >>= ppCartLabel

getPrice :: Product -> Text
getPrice p = fromMaybe "Inget pris" (getCartLabel p <|> productPrice p)

getSavePrice :: Product -> Maybe Text
getSavePrice p = Safe.headMay (productPotentialPromotions p) >>= ppSavePrice
