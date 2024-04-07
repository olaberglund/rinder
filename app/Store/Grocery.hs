module Store.Grocery (Product (..), Grocery (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe qualified as Maybe
import Data.Text
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS qualified as TLS
import Servant (FromHttpApiData)
import Servant.Client qualified as Client
import Servant.Client.Core qualified as Core
import Store.Ica.Client qualified as IcaClient
import Store.Ica.Response qualified as Ica
import Store.Willys.Client qualified as WillysClient
import Store.Willys.Response qualified as Willys
import Web.Internal.HttpApiData (parseQueryParam)

data Product = Product
    { productId :: !Text
    , productName :: !Text
    , productPrice :: !Text
    , productDescription :: !Text
    , productImageUrl :: !Text
    , productOffer :: !Text
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Grocery = Grocery
    { groceryGetOffers :: !(IO (Either Core.ClientError [Product]))
    , groceryGetSearchProduct :: !(Text -> IO (Either Core.ClientError [Product]))
    , groceryName :: !Text
    }

instance FromHttpApiData Grocery where
    parseQueryParam "willys" = Right willysGrocery
    parseQueryParam "ica" = Right icaGrocery
    parseQueryParam _ = Left "Invalid grocery store"

toProductWillys :: Willys.Product -> Product
toProductWillys p =
    Product
        { productId = Willys.getId p
        , productName = Willys.productName p
        , productPrice = Willys.getPrice p
        , productDescription = ""
        , productImageUrl =
            Maybe.fromMaybe
                ""
                (Willys.unImageUrl (Willys.productImage p))
        , productOffer = Maybe.fromMaybe "" (Willys.getSavePrice p)
        }

toProductWillys' :: Willys.Promotion -> Product
toProductWillys' = toProductWillys . Willys.unPromotion

willysGrocery :: Grocery
willysGrocery =
    Grocery
        { groceryGetOffers = runClientDefault willysBaseUrl (fmap toProductWillys' <$> WillysClient.fetchPromotions)
        , groceryGetSearchProduct = runClientDefault willysBaseUrl . fmap (fmap toProductWillys) . WillysClient.searchProduct
        , groceryName = "willys"
        }

willysBaseUrl :: Text
willysBaseUrl = "willys.se"

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

-- ICA

icaGrocery :: Grocery
icaGrocery =
    Grocery
        { groceryGetOffers = runClientDefault willysBaseUrl (fmap toProductWillys' <$> WillysClient.fetchPromotions)
        , groceryGetSearchProduct = runClientDefault baseUrl . fmap (fmap toProductIca) . IcaClient.searchProduct
        , groceryName = "ica"
        }
  where
    baseUrl = "handlaprivatkund.ica.se"

toProductIca :: Ica.Product -> Product
toProductIca p =
    Product
        { productId = Ica.productId p
        , productName = Ica.productName p
        , productPrice = Ica.unPrice (Ica.historyPriceCurrent (Ica.productPrice p))
        , productDescription = ""
        , productImageUrl = Ica.unImage (Ica.productImage p)
        , productOffer = ""
        }

runClientDefault :: Text -> Client.ClientM a -> IO (Either Client.ClientError a)
runClientDefault url action = do
    mgr <- TLS.newTlsManager
    baseUrl <- Client.parseBaseUrl (Text.unpack url)
    Client.runClientM action (addUserAgent $ Client.mkClientEnv mgr baseUrl)
