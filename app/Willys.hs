module Willys where

import Control.Applicative ((<|>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord qualified as Ord
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lucid (ToHtml (..), classes_, div_)
import Network.HTTP.Client.TLS qualified as TLS
import Safe qualified
import Servant (
    GenericMode (type (:-)),
    Get,
    JSON,
    NamedRoutes,
    Proxy (Proxy),
    QueryParam,
    type (:>),
 )
import Servant.Client ((//), (/:))
import Servant.Client qualified as Client
import Servant.Client.Core qualified as Core

type WillysAPI = NamedRoutes WillysRootAPI

{- FOURMOLU_DISABLE -}
data WillysRootAPI as = WillysRootAPI
    { getPromotionsEP 
        :: as 
        :- "search" 
        :> "campaigns" 
        :> "offline" 
        :> QueryParam "q" Int 
        :> QueryParam "type" Text 
        :> QueryParam "size" Int 
        :> Get '[JSON] PromotionResponse
    , searchProductsEP 
        :: as 
        :- "search" 
        :> "clean" 
        :> QueryParam "q" Text 
        :> QueryParam "size" Int 
        :> Get '[JSON] ProductResponse
    }
    deriving (Generic)

{- FOURMOLU_ENABLE -}
runClientDefault :: Client.ClientM a -> IO (Either Client.ClientError a)
runClientDefault action = do
    mgr <- TLS.newTlsManager
    baseUrl <- Client.parseBaseUrl "willys.se"
    Client.runClientM action (addUserAgent $ Client.mkClientEnv mgr baseUrl)

addUserAgent :: Client.ClientEnv -> Client.ClientEnv
addUserAgent env =
    env
        { Client.makeClientRequest =
            \b ->
                Client.defaultMakeClientRequest b
                    . Core.addHeader "User-Agent" userAgent
        }
  where
    userAgent :: Text
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

apiClient :: WillysRootAPI (Client.AsClientT Client.ClientM)
apiClient = Client.client (Proxy @WillysAPI)

fetchPromotions :: Client.ClientM [Promotion]
fetchPromotions =
    responseResults
        <$> ( apiClient
                // getPromotionsEP
                /: Just 2176
                /: Just "PERSONAL_GENERAL"
                /: Just 2000
            )

fetchProducts :: Text -> Client.ClientM [Product]
fetchProducts q =
    responseResults
        <$> (apiClient // searchProductsEP /: Just q /: Just 80)

type ProductResponse = Response Product

type PromotionResponse = Response Promotion

data Response a = Response
    { responseResults :: [a]
    , responsePagination :: Pagination
    }
    deriving (Generic, Show)

instance (Aeson.FromJSON a) => Aeson.FromJSON (Response a) where
    parseJSON = Aeson.genericParseJSON (customOptions "response")

instance (Aeson.ToJSON a) => Aeson.ToJSON (Response a) where
    toJSON = Aeson.genericToJSON (customOptions "response")

newtype Pagination = Pagination {unPagination :: Int}
    deriving (Generic, Show)

instance Aeson.FromJSON Pagination where
    parseJSON = Aeson.genericParseJSON paginationJSONOptions

instance Aeson.ToJSON Pagination where
    toJSON = Aeson.genericToJSON paginationJSONOptions

paginationJSONOptions :: Aeson.Options
paginationJSONOptions =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = const "numberOfPages"
        }

{- Promotion -}

newtype Promotion = Promotion {unProduct :: Product}
    deriving (Generic, Show, Ord)
    deriving newtype (Aeson.ToJSON)

instance Eq Promotion where
    (==) = (==) `on` unProduct

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

instance ToHtml Promotion where
    toHtml (Promotion prod) = do
        div_ [classes_ ["flex, space-x-2"]] $ do
            toHtml prod

    toHtmlRaw = toHtml

{- Product -}

getId :: Product -> Text
getId p =
    Text.filter (/= ' ') (productName p)
        <> Text.filter Char.isNumber (imageUrl (productImage p))

data Product = Product
    { productName :: Text
    , productImage :: ImageUrl
    , productPrice :: Maybe Text
    , productPotentialPromotions :: [PotentialPromotion]
    }
    deriving (Generic, Show)

instance Aeson.FromJSON Product where
    parseJSON = Aeson.genericParseJSON (customOptions "product")

instance Aeson.ToJSON Product where
    toJSON = Aeson.genericToJSON (customOptions "product")

customOptions :: String -> Aeson.Options
customOptions prefix =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = removePrefix prefix
        }

removePrefix :: String -> String -> String
removePrefix prefix = lowerCaseFirst . drop (length prefix)
  where
    lowerCaseFirst :: String -> String
    lowerCaseFirst (c : cs) = Char.toLower c : cs
    lowerCaseFirst s = s

instance Eq Product where
    (==) = (==) `on` getId

instance Ord Product where
    compare = Ord.comparing productImage

instance ToHtml Product where
    toHtml = toHtml . productName
    toHtmlRaw = toHtml

{- ImageUrl -}

newtype ImageUrl = ImageUrl {imageUrl :: Text}
    deriving (Generic, Show, Ord, Eq)

instance Aeson.FromJSON ImageUrl where
    parseJSON = Aeson.genericParseJSON (customOptions "image")

instance Aeson.ToJSON ImageUrl where
    toJSON = Aeson.genericToJSON (customOptions "image")

{- PotentialPromotion -}

data PotentialPromotion = PotentialPromotion
    {ppCartLabel :: Maybe Text, ppSavePrice :: Maybe Text}
    deriving (Generic, Show, Eq, Ord)

instance Aeson.FromJSON PotentialPromotion where
    parseJSON = Aeson.genericParseJSON (customOptions "pp")

instance Aeson.ToJSON PotentialPromotion where
    toJSON = Aeson.genericToJSON (customOptions "pp")

getCartLabel :: Product -> Maybe Text
getCartLabel p = Safe.headMay (productPotentialPromotions p) >>= ppCartLabel

getPrice :: Product -> Text
getPrice p = fromMaybe "Inget pris" (getCartLabel p <|> productPrice p)

getSavePrice :: Product -> Maybe Text
getSavePrice p = Safe.headMay (productPotentialPromotions p) >>= ppSavePrice
