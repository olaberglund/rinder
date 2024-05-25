module Store.Ica.Client (searchProduct) where

import qualified Data.Map           as Map
import           Data.Text
import           Servant            (NamedRoutes, Proxy (Proxy))
import           Servant.Client     ((//), (/:))
import qualified Servant.Client     as Client
import           Store.Ica.Api      (IcaRootApi (..))
import           Store.Ica.Response (Product, Response (unResponse))

apiClient :: IcaRootApi (Client.AsClientT Client.ClientM)
apiClient = Client.client (Proxy @(NamedRoutes IcaRootApi))

-- 1003827 is Ica kvantum Mobilia Lund
searchProduct :: Text -> Client.ClientM [Product]
searchProduct q =
    Map.elems
        . unResponse
        . unResponse
        <$> (apiClient // searchProductsEP /: 1003827 /: Just 50 /: Just 0 /: Just q)
