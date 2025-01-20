module Server.Shopping.Api (ShoppingApi (..)) where

import           GHC.Generics            (Generic)
import           Servant                 (Delete, FormUrlEncoded,
                                          GenericMode (type (:-)), Get, JSON,
                                          NoContent (..), NoFraming, Post,
                                          ReqBody, StreamGet, type (:>))
import           Servant.API             (Patch)
import           Servant.API.EventStream (EventStream)
import           Servant.HTML.Lucid      (HTML)
import           Server.Shopping.Html    (Note, ProductSearchList, Reordering,
                                          Search, ShoppingPage)
import qualified Store.Grocery           as Grocery

data ShoppingApi as = ShoppingApi
    { saPageEP :: !(as :- Get '[HTML] ShoppingPage)
    , saRemoveCheckedEP :: !(as :- "ta-bort" :> Delete '[HTML] NoContent)
    , saRemoveAllEP :: !(as :- "ta-bort-alla" :> Delete '[HTML] NoContent)
    -- ^ Server-sent events endpoint for the shopping list. A client connects
    --     and receives an event each time a user modifies the shopping list.
    , -- , saSseEP :: !(as :- "sse" :> ServerSentEvents (SourceIO XXX))

      saItemListEP ::
        !( as
            :- "produkter"
                :> ReqBody '[FormUrlEncoded] Search
                :> Post '[HTML] ProductSearchList
         )
    -- ^ The list of products matching the search query
    , saAddItemEP ::
        !( as
            :- "lagg-till"
                :> ReqBody '[JSON] Grocery.Product
                :> Post '[HTML] NoContent
         )
    -- ^ Add a product to the shopping list
    , saToggleItemEP ::
        !( as
            :- "toggla"
                :> ReqBody '[JSON] Grocery.Product
                :> Post '[HTML] NoContent
         )
    -- ^ Modify the note of an item
    , saModifyItemNoteEP ::
        !( as
            :- "anteckna"
                :> ReqBody '[FormUrlEncoded] Note
                :> Patch '[HTML] NoContent
         )
    -- ^ Reorder item
    , saReorderItemEP ::
        !( as
            :- "flytta"
                :> ReqBody '[JSON] Reordering
                :> Patch '[HTML] NoContent
         )
    }
    deriving stock (Generic)
