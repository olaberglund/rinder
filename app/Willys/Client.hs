module Willys.Client (
    runClientDefault,
    fetchPromotions,
    fetchProducts,
) where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Network.HTTP.Client.TLS qualified as TLS
import Servant (
    NamedRoutes,
    Proxy (Proxy),
 )
import Servant.Client ((//), (/:))
import Servant.Client qualified as Client
import Servant.Client.Core qualified as Core
import Willys.Api (WillysRootApi (..))
import Willys.Response (Product, Promotion, responseResults)

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

apiClient :: WillysRootApi (Client.AsClientT Client.ClientM)
apiClient = Client.client (Proxy @(NamedRoutes WillysRootApi))

fetchPromotions :: Client.ClientM [Promotion]
fetchPromotions =
    Maybe.fromMaybe []
        . responseResults
        <$> ( apiClient
                // getPromotionsEP
                /: Just 2176
                /: Just "PERSONAL_GENERAL"
                /: Just 2000
            )

fetchProducts :: Text -> Client.ClientM [Product]
fetchProducts q =
    Maybe.fromMaybe []
        . responseResults
        <$> (apiClient // searchProductsEP /: Just q /: Just 80)
