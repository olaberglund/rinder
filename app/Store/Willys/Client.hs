module Store.Willys.Client (
    fetchPromotions,
    searchProduct,
) where

import qualified Data.Maybe            as Maybe
import           Data.Text             (Text)
import           Servant               (NamedRoutes, Proxy (Proxy))
import           Servant.Client        ((//), (/:))
import qualified Servant.Client        as Client
import           Store.Willys.Api      (WillysRootApi (..))
import           Store.Willys.Response (Product, Promotion, responseResults)

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

searchProduct :: Text -> Client.ClientM [Product]
searchProduct q =
    Maybe.fromMaybe []
        . responseResults
        <$> (apiClient // searchProductsEP /: Just q /: Just 80)
