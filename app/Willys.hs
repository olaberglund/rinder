module Willys where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Function (on)
import Data.Ord (comparing)
import Data.Set (Set, union, unions)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Client hiding (Response)
import Prelude hiding (product)

-- TODO: Let products contain multiple urls, to remove duplicates in product list

type WillysAPI = NamedRoutes WillysRootAPI

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- "search" :> "campaigns" :> "offline" :> QueryParam "q" Int :> QueryParam "type" Text :> QueryParam "size" Int :> Get '[JSON] PromotionResponse,
    getProducts :: as :- "c" :> Capture "category" Text :> QueryParam "size" Int :> QueryParam "page" Int :> Get '[JSON] ProductResponse
  }
  deriving (Generic)

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchPromotions :: ClientM (Set Promotion)
fetchPromotions = do
  promotions <- (apiClient // getPromotions /: Just 2176 /: Just "PERSONAL_GENERAL" /: Just 2000)
  _ <- liftIO $ BS.writeFile "promotions.json" (encode promotions) -- to update the local file
  return $ results promotions

fetchProducts :: ClientM (Set Product)
fetchProducts = do
  prods <- liftIO (mapConcurrently (return . fetchProductsOfCategory) productHrefs)
  allprods <- unions <$> sequence prods
  _ <- liftIO $ BS.writeFile "products.json" (encode (Response allprods (Pagination (-1)))) -- to update the local file
  return allprods
  where
    fetchProductsOfCategory :: Text -> ClientM (Set Product)
    fetchProductsOfCategory href = do
      (Response res (Pagination tot)) <- fetchProductsOnPage href 0
      remainingCalls <- liftIO $ mapConcurrently (return . fetchProductsOnPage href) [1 .. tot - 1]
      remRes <- sequence remainingCalls
      return $ res `union` (unions $ results <$> remRes)

    fetchProductsOnPage :: Text -> Int -> ClientM (Response Product)
    fetchProductsOnPage href page = apiClient // getProducts /: href /: Just 100 /: Just page

productHrefs :: [Text]
productHrefs =
  [ "kott-chark-och-fagel",
    "frukt-och-gront",
    "mejeri-ost-och-agg",
    "fryst",
    "fisk-och-skaldjur",
    "skafferi"
  ]

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

data Response a = Response {results :: Set a, pagination :: Pagination}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Pagination = Pagination {numberOfPages :: Int}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Promotion = Promotion
  { price :: !(Maybe Text),
    product :: !Product,
    potentialPromotions :: ![PotentialPromotion]
  }
  deriving (Generic, Show, Ord)

instance Eq Promotion where
  (==) = (==) `on` product

instance ToJSON Promotion where
  toJSON (Promotion price product potentialPromotions) =
    object
      [ "price" .= price,
        "name" .= product.name, -- TODO: Find semigroup instance (toJSON product <> ... )
        "image" .= product.image,
        "potentialPromotions" .= potentialPromotions
      ]

newtype ImageUrl = ImageUrl {url :: Text}
  deriving (Generic, Show, Ord, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Product = Product {name :: Text, image :: ImageUrl}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Eq Product where
  p1 == p2 = p1.name == p2.name || p1.image == p2.image

instance Ord Product where
  compare = comparing image

instance ToHtml Product where
  toHtml = toHtml . name
  toHtmlRaw = toHtml

data PotentialPromotion = PotentialPromotion
  { promotionPrice :: !Float,
    qualifyingCount :: !(Maybe Int)
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON PotentialPromotion where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = \case "promotionPrice" -> "price"; s -> s}

instance FromJSON Promotion where
  parseJSON = withObject "Promotion" $ \v -> do
    productName :: Text <- v .: "name"
    imageUrl :: Text <- v .: "image" >>= (.: "url")
    Promotion <$> v .: "price" <*> pure (Product productName (ImageUrl imageUrl)) <*> v .: "potentialPromotions"

instance FromJSON PotentialPromotion where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "promotionPrice" -> "price"
            s -> s
        }

instance ToHtml Promotion where
  toHtml (Promotion _price prod _potentialPromotions) = do
    div_ [classes_ ["flex, space-x-2"]] $ do
      toHtml prod

  toHtmlRaw = toHtml
