module Server.App (app) where

import Servant (
    NamedRoutes,
    Proxy (Proxy),
    serveDirectoryWebApp,
 )
import Servant.Server
import Servant.Server.Generic (AsServer)
import Server.Api
import Server.Env (Env)
import Server.Handler
import Server.Html

app :: Env -> Application
app env =
    serveWithContext
        (Proxy @(NamedRoutes RootApi))
        (customFormatters :. EmptyContext)
        (server env)

server :: Env -> RootApi AsServer
server env =
    RootAPI
        { homePageEP = redirect "/split"
        , staticEP = serveDirectoryWebApp "static"
        , shoppingEP =
            ShoppingApi
                { shoppingPageEP = shoppingPageH env
                , productListEP = productListH addToShoppingList
                , addProductEP = addProductH env
                , toggleProductEP = toggleProductH env
                , removeCheckedEP = removeCheckedH env
                , removeAllEP = removeAllH env
                , sseEP = sseH env
                }
        , splitEP =
            SplitApi
                { splitPageEP = splitPageH env
                , newExpenseEP = newExpenseH env
                , settleUpEP = settleUpH env
                , editExpensePageEP = editExpensePageH env
                , saveExpenseEP = saveExpenseH env
                , removeExpenseEp = removeExpenseH env
                }
        }
