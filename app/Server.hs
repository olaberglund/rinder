module Server where

import App
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Html
import Lucid
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client (BaseUrl, ClientError, ClientM, mkClientEnv, runClientM)
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Willys hiding (getProducts)

newtype RootApi as = RootAPI
  { getProducts :: as :- Get '[HTML] HomePage
  }
  deriving (Generic)

type Api = NamedRoutes RootApi

app :: Env -> Application
app = serve (Proxy :: Proxy Api) . server

server :: Env -> RootApi AsServer
server env =
  RootAPI {getProducts = getProductsHandler env}

getProductsHandler :: Env -> Handler HomePage
getProductsHandler env = do
  res <- liftIO $ runClientDefault env.manager env.baseUrl fetchProducts
  case res of
    Right products -> return $ HomePage products
    Left _ -> return $ HomePage []

runClientDefault :: Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
runClientDefault mgr url action = runClientM action (addUserAgent $ mkClientEnv mgr url)
