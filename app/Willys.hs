module Willys where

import Control.Concurrent.Async
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req

userAgent :: Option scheme
userAgent = header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

fetchAllProducts' :: IO [JsonResponse ProductResponse]
fetchAllProducts' = mapConcurrently fetchProduct' productHrefs
  where
    fetchProduct' :: Text -> IO (JsonResponse ProductResponse)
    fetchProduct' = runReq defaultHttpConfig . fetchProduct

    productHrefs :: [Text]
    productHrefs =
      [ "kott-chark-och-fagel",
        "frukt-och-gront",
        "mejeri-ost-och-agg",
        "skafferi",
        "brod-och-kakor",
        "fryst",
        "fisk-och-skaldjur",
        "vegetariskt"
      ]

fetchProduct :: Text -> Req (JsonResponse ProductResponse)
fetchProduct href =
  req
    GET
    (https "willys.se" /: "c" /: href)
    NoReqBody
    jsonResponse
    ("size" =: (2000 :: Int) <> userAgent)

fetchPromotions :: Req (JsonResponse PromotionResponse)
fetchPromotions =
  req
    GET
    (https "willys.se" /: "search" /: "campaigns" /: "offline")
    NoReqBody
    jsonResponse
    ("q" =: (2176 :: Int) <> "type" =: ("PERSONAL_GENERAL" :: Text) <> "size" =: (2000 :: Int))

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
