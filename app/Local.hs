module Local where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Servant
import Servant.Client (ClientM, client, (//))
import Servant.Client.Core.HasClient (AsClientT)
import Servant.Server.Generic (AsServer)
import Willys (Product, ProductResponse, Promotion, PromotionResponse, Response (..))

data LocalRootApi as = LocalRootAPI
  { getProducts :: as :- Get '[JSON] ProductResponse,
    getPromotions :: as :- Get '[JSON] PromotionResponse
  }
  deriving (Generic)

app :: Application
app = serve (Proxy :: Proxy (NamedRoutes LocalRootApi)) server

server :: LocalRootApi AsServer
server =
  LocalRootAPI
    { getProducts = liftIO (print "WRONG!") >> getProductsHandler,
      getPromotions = liftIO (print "CORRECT") >> getPromotionsHandler
    }

getProductsHandler :: Handler ProductResponse
getProductsHandler = do
  res <- liftIO $ BS.readFile "product-category.json"
  case eitherDecodeStrict res of
    Right (Response products) -> return $ Response products
    Left err -> liftIO (print err) >> return (Response [])

getPromotionsHandler :: Handler PromotionResponse
getPromotionsHandler = do
  res <- liftIO $ BS.readFile "promotions.json"
  case eitherDecodeStrict res of
    Right (Response promotions) -> return $ Response promotions
    Left err -> liftIO (print err) >> return (Response [])

--------

apiClient :: LocalRootApi (AsClientT ClientM)
apiClient = client (Proxy @(NamedRoutes LocalRootApi))

fetchProducts :: ClientM [Product]
fetchProducts = results <$> (apiClient // getProducts)

fetchPromotions :: ClientM [Promotion]
fetchPromotions = liftIO (print "PROMOTIONS?") >> results <$> (apiClient // getPromotions)
