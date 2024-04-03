module Willys.API (WillysRootApi, getPromotionsEP, searchProductsEP) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (
    GenericMode (type (:-)),
    Get,
    JSON,
    QueryParam,
    (:-),
    (:>),
    type (:>),
 )
import Willys.Response (ProductResponse, PromotionResponse)

{- FOURMOLU_DISABLE -}
data WillysRootApi as = WillysRootAPI
    { getPromotionsEP 
         :: !(as 
        :- "search" 
        :> "campaigns" 
        :> "offline" 
        :> QueryParam "q" Int 
        :> QueryParam "type" Text 
        :> QueryParam "size" Int 
        :> Get '[JSON] PromotionResponse)
    , searchProductsEP 
        :: !(as 
        :- "search" 
        :> "clean" 
        :> QueryParam "q" Text 
        :> QueryParam "size" Int 
        :> Get '[JSON] ProductResponse)
    }
    deriving stock (Generic)
{- FOURMOLU_ENABLE -}
