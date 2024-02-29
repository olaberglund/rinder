module Willys where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https))

userAgent :: Text
userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

type WillysAPI = NamedRoutes WillysRootAPI

-- fetchPromotions :: Req (JsonResponse PromotionResponse)
-- fetchPromotions =
--   req
--     GET
--     (https "willys.se" /: "search" /: "campaigns" /: "offline")
--     NoReqBody
--     jsonResponse
--     ("q" =: (2176 :: Int) <> "type" =: ("PERSONAL_GENERAL" :: Text) <> "size" =: (2000 :: Int))

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- QueryParam "q" Int :> QueryParam "type" Text :> QueryParam "size" Int :> Get '[JSON] [Promotion],
    getProducts :: as :- Capture "category" Text :> QueryParam "size" Int :> Get '[JSON] [Product]
  }
  deriving (Generic)

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

newtype Response a = Response
  { results :: [a]
  }
  deriving (Generic, Show)

data Promotion = Promotion
  { price :: !(Maybe Text),
    product :: !Product,
    potentialPromotions :: ![PotentialPromotion]
  }
  deriving (Generic, Show, Ord, Eq)

newtype Product = Product {name :: Text}
  deriving (Generic, Show, Eq, Ord)

data PotentialPromotion = PotentialPromotion
  { promotionPrice :: !Float,
    qualifyingCount :: !(Maybe Int)
  }
  deriving (Generic, Show, Ord, Eq)

instance (FromJSON a) => FromJSON (Response a)

instance FromJSON Promotion where
  parseJSON (Object v) = do
    productName :: Text <- v .: "name"
    Promotion <$> v .: "price" <*> pure (Product productName) <*> v .: "potentialPromotions"
  parseJSON _ = mempty

instance FromJSON Product

instance FromJSON PotentialPromotion where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "promotionPrice" -> "price"
            s -> s
        }

instance ToJSON PotentialPromotion where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Product where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Promotion where
  toEncoding = genericToEncoding defaultOptions

instance (ToJSON a) => ToJSON (Response a) where
  toEncoding = genericToEncoding defaultOptions
