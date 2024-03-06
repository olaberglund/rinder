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
server = RootAPI . getPromotionsHandler

getProductsHandler :: Env a -> Handler HomePage
getProductsHandler env =
  liftIO (runClientDefault env.manager env.baseUrl (return [])) >>= \case
    Right products -> return $ HomePage products
    Left _err -> return (HomePage [])

getPromotionsHandler :: Env a -> Handler HomePage
getPromotionsHandler env =
  liftIO (runClientDefault env.manager env.baseUrl env.fetchPromotions) >>= \case
    Right promos -> return $ HomePage promos
    Left err -> liftIO (print err) >> return (HomePage [])

runClientDefault :: Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
runClientDefault mgr url action = runClientM action (addUserAgent $ mkClientEnv mgr url)

productionEnv :: IO (Env Production)
productionEnv = do
  mgr <- newTlsManager
  url <- parseBaseUrl "https://www.willys.se"
  return $ Env mgr url Willys.fetchProducts Willys.fetchPromotions

localEnv :: IO (Env Local)
localEnv = do
  mgr <- newManager defaultManagerSettings
  url <- parseBaseUrl "localhost:8082"
  return $ Env mgr url Local.fetchProducts Local.fetchPromotions

addUserAgent :: ClientEnv -> ClientEnv
addUserAgent env = env {makeClientRequest = \b -> defaultMakeClientRequest b . Core.addHeader "User-Agent" userAgent}
  where
    userAgent :: Text
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"
