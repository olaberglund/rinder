module Server.Shopping.Api (ShoppingApi (..)) where

import GHC.Generics (Generic)
import Servant (
    Delete,
    FormUrlEncoded,
    GenericMode (type (:-)),
    Get,
    JSON,
    NoContent (..),
    NoFraming,
    Post,
    ReqBody,
    StreamGet,
    type (:>),
 )
import Servant.API.EventStream (EventSource, EventStream)
import Servant.HTML.Lucid (HTML)
import Server.Shopping.Html (
    ProductSearchList,
    Search,
    ShoppingPage,
 )
import Store.Willys.Response (Product (..))

data ShoppingApi as = ShoppingApi
    { shoppingPageEP :: !(as :- Get '[HTML] ShoppingPage)
    , removeCheckedEP :: !(as :- "ta-bort" :> Delete '[HTML] NoContent)
    , removeAllEP :: !(as :- "ta-bort-alla" :> Delete '[HTML] NoContent)
    , sseEP :: !(as :- "sse" :> StreamGet NoFraming EventStream EventSource)
    -- ^ Server-sent events endpoint for the shopping list. A client connects
    --     and receives an event each time a user modifies the shopping list.
    , productListEP ::
        !( as
            :- "produkter"
                :> ReqBody '[FormUrlEncoded] Search
                :> Post '[HTML] ProductSearchList
         )
    -- ^ The list of products matching the search query
    , addProductEP ::
        !( as
            :- "lagg-till"
                :> ReqBody '[JSON] Product
                :> Post '[HTML] NoContent
         )
    -- ^ Add a product to the shopping list
    , toggleProductEP ::
        !( as
            :- "toggla"
                :> ReqBody '[JSON] Product
                :> Post '[HTML] NoContent
         )
    -- ^ Toggle a product in the shopping list
    }
    deriving stock (Generic)
