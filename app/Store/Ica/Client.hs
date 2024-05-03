module Store.Ica.Client where

import qualified Data.Map           as Map
import           Data.Text
import           Servant            (NamedRoutes, Proxy (Proxy))
import           Servant.Client     ((//), (/:))
import qualified Servant.Client     as Client
import           Store.Ica.Api
import           Store.Ica.Response

apiClient :: IcaRootApi (Client.AsClientT Client.ClientM)
apiClient = Client.client (Proxy @(NamedRoutes IcaRootApi))

-- 1003827 is Ica kvantum Mobilia Lund
searchProduct :: Text -> Client.ClientM [Product]
searchProduct q =
    Map.elems
        . unResponse
        . unResponse
        <$> (apiClient // searchProductsEP /: 1003827 /: Just 50 /: Just 0 /: Just q)
