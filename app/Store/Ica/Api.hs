module Store.Ica.Api where

import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Servant            (GenericMode (type (:-)), Get, JSON,
                                     QueryParam, (:-), (:>))
import           Servant.API        (Capture)
import           Store.Ica.Response

{- FOURMOLU_DISABLE -}
data IcaRootApi as = IcaRootAPI
    { searchProductsEP
        :: !(as
        :- "stores"
        :> Capture "storeId" Int
        :> "api"
        :> "v5"
        :> "products"
        :> "search"
        :> QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> QueryParam "term" Text
        :> Get '[JSON] EntityResponse)
    }
    deriving stock (Generic)
{- FOURMOLU_ENABLE -}

-- host = handlaprivatkund.ica.se
-- filename =  /stores/1003827/api/v5/products/search
-- queries = limit 50, offset 0, term tomat
