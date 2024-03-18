module Willys where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (zipWith4)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lucid
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client hiding (Response)
import Servant.Client.Core qualified as Core
import Web.FormUrlEncoded (FromForm, fromForm, parseAll, parseUnique)
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
    price :: Maybe Text <- v .: "price"
    potentialPromotions :: [PotentialPromotion] <- v .: "potentialPromotions"
    return $ Promotion (Product productName (ImageUrl imageUrl) price potentialPromotions)

instance ToHtml Promotion where
  toHtml (Promotion prod) = do
    div_ [classes_ ["flex, space-x-2"]] $ do
      toHtml prod

  toHtmlRaw = toHtml

{- Product -}

getId :: Product -> Text
getId p = Text.filter (/= ' ') p.name <> Text.filter isNumber p.image.url

data Product = Product
  { name :: Text,
    image :: ImageUrl,
    price :: Maybe Text,
    potentialPromotions :: [PotentialPromotion]
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Eq Product where
  (==) = (==) `on` getId

instance Ord Product where
  compare = comparing image

instance ToHtml Product where
  toHtml = toHtml . (.name)
  toHtmlRaw = toHtml

instance FromForm Product where
  fromForm form =
    Product
      <$> (parseUnique "name" form)
      <*> (ImageUrl <$> parseUnique "url" form)
      <*> pure Nothing
      <*> pure []

instance FromForm [Product] where
  fromForm form = do
    names <- parseAll "names" form
    urls <- parseAll "urls" form
    return $ zipWith4 Product names (map ImageUrl urls) (repeat Nothing) (repeat [])

{- ImageUrl -}

newtype ImageUrl = ImageUrl {url :: Text}
  deriving (Generic, Show, Ord, Eq)
  deriving anyclass (FromJSON, ToJSON)

{- PotentialPromotion -}

data PotentialPromotion = PotentialPromotion
  {cartLabel :: Maybe Text, savePrice :: Maybe Text}
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

getCartLabel :: Product -> Maybe Text
getCartLabel p = safeHead (potentialPromotions p) >>= cartLabel

getPrice :: Product -> Text
getPrice p = fromMaybe "Inget pris" (getCartLabel p <|> price p)

getSavePrice :: Product -> Maybe Text
getSavePrice p = safeHead (potentialPromotions p) >>= savePrice

{- ToHtml instances -}
