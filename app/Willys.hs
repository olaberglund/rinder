module Willys where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Client hiding (Response)

type WillysAPI = NamedRoutes WillysRootAPI

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- QueryParam "q" Int :> QueryParam "type" Text :> QueryParam "size" Int :> Get '[JSON] PromotionResponse,
    getProducts :: as :- Capture "category" Text :> QueryParam "size" Int :> Get '[JSON] ProductResponse
  }
  deriving (Generic)

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchPromotions :: ClientM [Promotion]
fetchPromotions = results <$> (apiClient // getPromotions /: Just 2176 /: Just "PERSONAL_GENERAL" /: Just 2000)

fetchProducts :: ClientM [Product]
fetchProducts =
  liftIO (mapConcurrently (return . fetchProduct) productHrefs)
    >>= fmap concat . sequence
  where
    fetchProduct :: Text -> ClientM [Product]
    fetchProduct href = results <$> (apiClient // getProducts /: href /: Just 2000)

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

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

newtype Response a = Response {results :: [a]}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Promotion = Promotion
  { price :: !(Maybe Text),
    product :: !Product,
    potentialPromotions :: ![PotentialPromotion]
  }
  deriving (Generic, Show, Ord, Eq)
  deriving anyclass (ToJSON)

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
  deriving anyclass (ToJSON)

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
