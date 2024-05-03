-- | This module defines the API for the server.
module Server.Api (RootApi (..), PagesApi (..)) where

import           GHC.Generics        (Generic)
import           Inter.Language      (Language)
import           Servant             (Capture, GenericMode (type (:-)), Get,
                                      NamedRoutes, NoContent (..), Raw,
                                      type (:>))
import           Servant.HTML.Lucid  (HTML)
import           Server.Shopping.Api (ShoppingApi)
import           Server.Split.Api    (SplitApi)
import           Store.Grocery       (Grocery)

{- | The root API which contains the sub-apis for the shopping list and
split pages.

All endpoints are strict fields so that an error is raised if they are not
provided when constructing the API.
-}
{- FOURMOLU_DISABLE -}
data RootApi as = RootApi
    { homePageEP :: !(as :- Get '[HTML] NoContent)
    -- | static files are served from the "static" directory
    , staticEP   :: !(as :- "static" :> Raw)
    , pagesEP    :: !(as :- Capture "lang" Language :> NamedRoutes PagesApi)
    }
    deriving stock (Generic)

data PagesApi as = PagesApi {
     languageHomePageEP :: !(as :- Get '[HTML] NoContent),
    -- | entry point to the shopping list page
     shoppingEP :: !(as :- "inkop" :> Capture "grocery" Grocery :> NamedRoutes ShoppingApi)
    -- | entry point to the expense split page
    , splitEP :: !(as :- "split" :> NamedRoutes SplitApi)
    }
    deriving stock (Generic)


data TodoApi as = TodoApi
    { todoPageEP :: !(as :- Get '[HTML] NoContent)
    }
    deriving stock (Generic)
{- FOURMOLU_ENABLE -}
