module Server.Api (RootApi (..), ShoppingApi (..), SplitApi (..)) where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (
    Capture,
    Delete,
    FormUrlEncoded,
    GenericMode (type (:-)),
    Get,
    JSON,
    NamedRoutes,
    NoContent (..),
    NoFraming,
    Patch,
    Post,
    Raw,
    ReqBody,
    StreamGet,
    type (:>),
 )
import Servant.API.EventStream (EventSource, EventStream)
import Servant.HTML.Lucid (HTML)
import Server.Html (
    EditExpensePage,
    ProductSearchList,
    Search,
    ShoppingItem,
    ShoppingPage,
    SplitPage,
    Transactions,
 )
import Splitvajs (
    ExpenseForm,
 )
import Willys.Response (
    Product (..),
 )

{- FOURMOLU_DISABLE -}
data RootApi as = RootAPI
    { homePageEP :: !(as :- Get '[HTML] NoContent)
    , staticEP :: !(as :- "static" :> Raw)
    , shoppingEP :: !(as :- "inkop" :> NamedRoutes ShoppingApi)
    , splitEP :: !(as :- "split" :> NamedRoutes SplitApi)
    }
    deriving stock (Generic)

data ShoppingApi as = ShoppingApi
    { shoppingPageEP :: !(as :- Get '[HTML] ShoppingPage)
    , removeCheckedEP :: !(as :- "ta-bort" :> Delete '[HTML] [ShoppingItem])
    , removeAllEP :: !(as :- "ta-bort-alla" :> Delete '[HTML] [ShoppingItem])
    , sseEP :: !(as :- "sse" :> StreamGet NoFraming EventStream EventSource)
    , productListEP 
        :: !(as 
        :- "produkter" 
        :> ReqBody '[FormUrlEncoded] Search 
        :> Post '[HTML] ProductSearchList)
    , addProductEP 
        :: !(as :- "lagg-till" 
        :> ReqBody '[JSON] Product 
        :> Post '[HTML] [ShoppingItem])
    , toggleProductEP 
        :: !(as :- "toggla" 
        :> ReqBody '[JSON] Product 
        :> Post '[HTML] NoContent)
    }
    deriving stock (Generic)

data SplitApi as = SplitApi
    { splitPageEP :: !(as :- Get '[HTML] SplitPage)
    , settleUpEP :: !(as :- "gor-upp" :> Post '[HTML] Transactions)
    , removeExpenseEp 
        :: !(as 
        :- "ta-bort" 
        :> Capture "id" UUID 
        :> Delete '[HTML] NoContent)
    , newExpenseEP 
        :: !(as :- "lagg-till" 
        :> ReqBody '[FormUrlEncoded] ExpenseForm 
        :> Post '[HTML] Transactions)
    , editExpensePageEP 
        :: !(as 
        :- "redigera" 
        :> Capture "id" UUID 
        :> Get '[HTML] EditExpensePage)
    , saveExpenseEP 
        :: !(as 
        :- "spara" 
        :> Capture "id" UUID 
        :> ReqBody '[FormUrlEncoded] ExpenseForm 
        :> Patch '[HTML] EditExpensePage)
    }
    deriving stock (Generic)

{- FOURMOLU_ENABLE -}
