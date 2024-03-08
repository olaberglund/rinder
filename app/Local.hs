module Local where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Servant
import Servant.Client (ClientM, client, (//))
import Servant.Client.Core.HasClient (AsClientT)
import Servant.Server.Generic (AsServer)
import Willys (Pagination (Pagination), Product, ProductResponse, Promotion, PromotionResponse, Response (..))

port :: Int
port = 8082

data LocalRootApi as = LocalRootAPI
  { getProducts :: as :- "products" :> Get '[JSON] ProductResponse,
    getPromotions :: as :- "promotions" :> Get '[JSON] PromotionResponse
  }
  deriving (Generic)

app :: Application
app = serve (Proxy :: Proxy (NamedRoutes LocalRootApi)) server

server :: LocalRootApi AsServer
server =
  LocalRootAPI
    { getProducts = getProductsHandler,
      getPromotions = getPromotionsHandler
    }

getProductsHandler :: Handler ProductResponse
getProductsHandler = handler "products.json"

getPromotionsHandler :: Handler PromotionResponse
getPromotionsHandler = handler "promotions.json"

handler :: (Ord a, FromJSON a) => FilePath -> Handler (Response a)
handler path = do
  res <- liftIO $ BS.readFile path
  case eitherDecodeStrict res of
    Right (Response promotions p) -> return $ Response promotions p
    Left err -> liftIO (print err) >> return (Response Set.empty (Pagination (-1)))

--------

apiClient :: LocalRootApi (AsClientT ClientM)
apiClient = client (Proxy @(NamedRoutes LocalRootApi))

fetchProducts :: ClientM (Set Product)
fetchProducts = results <$> (apiClient // getProducts)

fetchPromotions :: ClientM (Set Promotion)
fetchPromotions = results <$> (apiClient // getPromotions)
