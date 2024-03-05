module Server where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Html
import Local qualified
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client
import Servant.Client.Core qualified as Core
import Servant.Server.Generic (AsServer)
import Willys qualified

app :: Env a -> Application
app = serve (Proxy :: Proxy Api) . server

server :: Env a -> RootApi AsServer
server = RootAPI . getProductsHandler

getProductsHandler :: Env a -> Handler HomePage
getProductsHandler env =
  liftIO (runClientDefault env.manager env.baseUrl env.fetchProducts) >>= \case
    Right products -> return $ HomePage products
    Left err -> liftIO (print err) >> return (HomePage [])

runClientDefault :: Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
runClientDefault mgr url action = runClientM action (addUserAgent $ mkClientEnv mgr url)

productionEnv :: IO (Env Production)
productionEnv = do
  mgr <- newTlsManager
  url <- parseBaseUrl "https://www.willys.se"
  return $ Env mgr url Willys.fetchProducts

localEnv :: IO (Env Local)
localEnv = do
  mgr <- newManager defaultManagerSettings
  url <- parseBaseUrl "localhost:8082"
  return $ Env mgr url Local.fetchProducts

addUserAgent :: ClientEnv -> ClientEnv
addUserAgent env = env {makeClientRequest = \b -> defaultMakeClientRequest b . Core.addHeader "User-Agent" userAgent}
  where
    userAgent :: Text
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"
