module Local where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Servant
import Servant.Client (ClientM)
import Servant.Server.Generic (AsServer)
import Willys (Product, ProductResponse, Response (..))

newtype LocalRootApi as = LocalRootAPI
  { getProducts :: as :- Get '[JSON] ProductResponse
  }
  deriving (Generic)

fetchProducts :: ClientM [Product]
fetchProducts = return []

app :: Application
app = serve (Proxy :: Proxy (NamedRoutes LocalRootApi)) server

server :: LocalRootApi AsServer
server = LocalRootAPI $ do
  res <- liftIO $ BS.readFile "products.json"
  case eitherDecodeStrict res of
    Right (Response products) -> return $ Response products
    Left err -> liftIO (print err) >> return (Response [])
