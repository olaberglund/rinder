module Willys where

import Data.Aeson
import Data.Function (on)
import Data.Ord (comparing)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client hiding (Response)
import Servant.Client.Core qualified as Core
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Prelude hiding (product)

type WillysAPI = NamedRoutes WillysRootAPI

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- "search" :> "campaigns" :> "offline" :> QueryParam "q" Int :> QueryParam "type" Text :> QueryParam "size" Int :> Get '[JSON] PromotionResponse,
    searchProducts :: as :- "search" :> "clean" :> QueryParam "q" Text :> QueryParam "size" Int :> Get '[JSON] ProductResponse
  }
  deriving (Generic)

runClientDefault :: ClientM a -> IO (Either ClientError a)
runClientDefault action = do
  mgr <- newTlsManager
  baseUrl <- parseBaseUrl "willys.se"
  runClientM action (addUserAgent $ mkClientEnv mgr baseUrl)

addUserAgent :: ClientEnv -> ClientEnv
addUserAgent env = env {makeClientRequest = \b -> defaultMakeClientRequest b . Core.addHeader "User-Agent" userAgent}
  where
    userAgent :: Text
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchPromotions :: ClientM [Promotion]
fetchPromotions = results <$> (apiClient // getPromotions /: Just 2176 /: Just "PERSONAL_GENERAL" /: Just 2000)

fetchProducts :: Text -> ClientM [Product]
fetchProducts q = results <$> (apiClient // searchProducts /: Just q /: Just 80)

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

data Response a = Response {results :: [a], pagination :: Pagination}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Pagination = Pagination {numberOfPages :: Int}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

{- Promotion -}

data Promotion = Promotion {product :: !Product}
  deriving (Generic, Show, Ord)

instance Eq Promotion where
  (==) = (==) `on` product

instance ToJSON Promotion where
  toJSON (Promotion product) =
    object
      ["name" .= product.name, "image" .= product.image]

instance FromJSON Promotion where
  parseJSON = withObject "Promotion" $ \v -> do
    productName :: Text <- v .: "name"
    imageUrl :: Text <- v .: "image" >>= (.: "url")
    return $ Promotion (Product productName (ImageUrl imageUrl))

instance ToHtml Promotion where
  toHtml (Promotion prod) = do
    div_ [classes_ ["flex, space-x-2"]] $ do
      toHtml prod

  toHtmlRaw = toHtml

{- Product -}

data Product = Product
  { name :: Text,
    image :: ImageUrl
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Eq Product where
  p1 == p2 = p1.image == p2.image

instance Ord Product where
  compare = comparing image

instance ToHtml Product where
  toHtml = toHtml . (.name)
  toHtmlRaw = toHtml

instance FromForm Product where
  fromForm form = Product <$> (parseUnique "name" form) <*> (ImageUrl <$> parseUnique "url" form)

{- ImageUrl -}

newtype ImageUrl = ImageUrl {url :: Text}
  deriving (Generic, Show, Ord, Eq)
  deriving anyclass (FromJSON, ToJSON)

{- Flattened Product -}

data FlattenedProduct = FlattenedProduct
  { name :: Text,
    imgUrl :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
