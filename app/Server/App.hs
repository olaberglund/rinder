{- | This module defines the server application which is simply a composition of
the API and the handlers.
-}
module Server.App (app) where

import Data.Text.Encoding qualified as Text
import Inter.Language (Language (..), mkHref)
import Servant (
    NamedRoutes,
    Proxy (Proxy),
    serveDirectoryWebApp,
 )
import Servant.Server
import Servant.Server.Generic (AsServer)
import Server.Api
import Server.Env (Env)
import Server.Shopping.Api
import Server.Shopping.Handler
import Server.Shopping.Html
import Server.Split.Api
import Server.Split.Handler
import Server.Utils.Handler

app :: Env -> Application
app env =
    serveWithContext
        (Proxy @(NamedRoutes RootApi))
        (customFormatters :. EmptyContext)
        (server env)

server :: Env -> RootApi AsServer
server env =
    RootApi
        { homePageEP = redirect (home SE)
        , staticEP = serveDirectoryWebApp "static"
        , pagesEP = \lang ->
            PagesApi
                { languageHomePageEP = redirect (home lang)
                , shoppingEP =
                    ShoppingApi
                        { shoppingPageEP = shoppingPageH env lang
                        , productListEP = productListH lang (addToShoppingList lang)
                        , addProductEP = addProductH env
                        , toggleProductEP = toggleProductH env
                        , removeCheckedEP = removeCheckedH env
                        , removeAllEP = removeAllH env
                        , sseEP = sseH env
                        }
                , splitEP =
                    SplitApi
                        { splitPageEP = splitPageH env lang
                        , newExpenseEP = newExpenseH env lang
                        , settleUpEP = settleUpH env lang
                        , editExpensePageEP = editExpensePageH env lang
                        , saveExpenseEP = saveExpenseH env lang
                        , removeExpenseEp = removeExpenseH env
                        }
                }
        }
  where
    home lang = Text.encodeUtf8 $ mkHref lang "/split"
