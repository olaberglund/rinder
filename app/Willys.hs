module Willys where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Client hiding (Response)

type WillysAPI = NamedRoutes WillysRootAPI

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- "search" :> "campaigns" :> "offline" :> QueryParam "q" Int :> QueryParam "type" Text :> QueryParam "size" Int :> Get '[JSON] PromotionResponse,
    getProducts :: as :- "c" :> Capture "category" Text :> QueryParam "size" Int :> QueryParam "page" Int :> Get '[JSON] ProductResponse
  }
  deriving (Generic)

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchPromotions :: ClientM [Promotion]
fetchPromotions = results <$> (apiClient // getPromotions /: Just 2176 /: Just "PERSONAL_GENERAL" /: Just 2000)

fetchProducts :: ClientM [Product]
fetchProducts = do
  prods <- liftIO (mapConcurrently (return . fetchProductsOfCategory) productHrefs)
  allprods <- fmap concat $ sequence prods
  _ <- liftIO $ BS.writeFile "test.json" (encode (Response allprods (Pagination (-1))))
  return allprods
  where
    fetchProductsOfCategory :: Text -> ClientM [Product]
    fetchProductsOfCategory href = do
      (Response res (Pagination tot)) <- apiClient // getProducts /: href /: Just 100 /: Just 0
      rest <- liftIO $ mapConcurrently (\page -> return $ apiClient // getProducts /: href /: Just 100 /: Just page) [1 .. tot - 1]
      responses <- sequence rest
      liftIO $ print responses
      return $ res ++ concat (map results responses)

productHrefs :: [Text]
productHrefs =
  [ "kott-chark-och-fagel",
    "frukt-och-gront",
    "mejeri-ost-och-agg",
    "skafferi",
    "brod-och-kakor",
    "fryst",
    "fisk-och-skaldjur",
    "vegetariskt",
    "skafferi"
  ]

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

data Response a = Response {results :: [a], pagination :: Pagination}
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
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Promotion where
  toJSON (Promotion price (Product nm) potentialPromotions) =
    object
      [ "price" .= price,
        "name" .= nm,
        "potentialPromotions" .= potentialPromotions
      ]

newtype Product = Product {name :: Text}
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance ToHtml Product where
  toHtml (Product nm) = toHtml nm
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
    Promotion <$> v .: "price" <*> pure (Product productName) <*> v .: "potentialPromotions"

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
