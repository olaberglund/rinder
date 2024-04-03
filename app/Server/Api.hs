-- | This module defines the API for the server.
module Server.Api (
    RootApi (..),
    ShoppingApi (..),
    SplitApi (..),
    PagesApi (..),
) where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Inter.Language (Language)
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
    ShoppingItems,
    ShoppingPage,
    SplitPage,
    Transactions,
 )
import Split (ExpenseForm)
import Willys.Response (Product (..))

{- | The root API which contains the sub-apis for the shopping list and
split pages.

All endpoints are strict fields so that an error is raised if they are not
provided when constructing the API.
-}
{- FOURMOLU_DISABLE -}
data RootApi as = RootApi
    { homePageEP :: !(as :- Get '[HTML] NoContent)
    -- | static files are served from the "static" directory
    , staticEP :: !(as :- "static" :> Raw)
    , pagesEP :: !(as :- Capture "lang" Language :> NamedRoutes PagesApi)
    }
    deriving stock (Generic)

data PagesApi as = PagesApi {
     languageHomePageEP :: !(as :- Get '[HTML] NoContent),
    -- | entry point to the shopping list page
     shoppingEP :: !(as :- "inkop" :> NamedRoutes ShoppingApi)
    -- | entry point to the expense split page
    , splitEP :: !(as :- "split" :> NamedRoutes SplitApi)
    }
    deriving stock (Generic)

data ShoppingApi as = ShoppingApi
    { shoppingPageEP :: !(as :- Get '[HTML] ShoppingPage)
    , removeCheckedEP :: !(as :- "ta-bort" :> Delete '[HTML] ShoppingItems)
    , removeAllEP :: !(as :- "ta-bort-alla" :> Delete '[HTML] ShoppingItems)
    {- | Server-sent events endpoint for the shopping list. A client connects 
    and receives an event each time a user modifies the shopping list. -}
    , sseEP :: !(as :- "sse" :> StreamGet NoFraming EventStream EventSource)
    -- | The list of products matching the search query
    , productListEP 
        :: !(as 
        :- "produkter" 
        :> ReqBody '[FormUrlEncoded] Search 
        :> Post '[HTML] ProductSearchList)
    -- | Add a product to the shopping list
    , addProductEP 
        :: !(as :- "lagg-till" 
        :> ReqBody '[JSON] Product 
        :> Post '[HTML] ShoppingItems)
    -- | Toggle a product in the shopping list
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
        -- | The UUID of the expense to remove
        :> Capture "id" UUID  
        -- | Redirects upon successfull deletion
        :> Delete '[HTML] NoContent) 
    , newExpenseEP 
        :: !(as :- "lagg-till" 
        -- | Create a new expense from the expense form
        :> ReqBody '[FormUrlEncoded] ExpenseForm  
        -- | Swaps in the list of transactions, including the new expense
        :> Post '[HTML] Transactions)
    , editExpensePageEP 
        :: !(as 
        :- "redigera" 
        -- | The UUID of the expense to edit
        :> Capture "id" UUID 
        :> Get '[HTML] EditExpensePage)
    , saveExpenseEP 
        :: !(as 
        :- "spara" 
        -- | The UUID of the expense to edit
        :> Capture "id" UUID 
        -- | The updated expense
        :> ReqBody '[FormUrlEncoded] ExpenseForm 
        -- | Updated expense page with feedback and updated expense details
        :> Patch '[HTML] EditExpensePage)
    }
    deriving stock (Generic)
{- FOURMOLU_ENABLE -}
