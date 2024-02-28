module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client (AsClientT, BaseUrl (BaseUrl), ClientM, Scheme (Https), client, mkClientEnv, runClientM, (//), (/:))
import Willys
import Prelude hiding (putStr)

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM fetchAllProducts (mkClientEnv manager' (BaseUrl Https "willys.se" 80 ""))
  print res

type WillysAPI = NamedRoutes WillysRootAPI

data WillysRootAPI as = WillysRootAPI
  { getPromotions :: as :- "search" :> "campaigns" :> "offline" :> Capture "q" Int :> Get '[JSON] [Promotion],
    getProducts :: as :- "c" :> Capture "category" Text :> Get '[JSON] [Product]
  }
  deriving (Generic)

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchAllProducts :: ClientM [Product]
fetchAllProducts = do
  products <- liftIO $ mapConcurrently (\href -> return $ apiClient // getProducts /: href) productHrefs
  concat <$> sequence products
  where
    productHrefs :: [Text]
    productHrefs =
      [ "kott-chark-och-fagel",
        "frukt-och-gront",
        "mejeri-ost-och-agg",
        "skafferi",
        "brod-och-kakor",
        "fryst",
        "fisk-och-skaldjur",
        "vegetariskt"
      ]

-- main :: IO ()
-- main = run 8081 app

-- type RinderAPI = NamedRoutes RootAPI -- "products" :> Get '[JSON] [Product]

-- newtype RootAPI as = RootAPI {getProducts :: as :- Get '[JSON] [Product]}
--   deriving (Generic)

-- server :: RootAPI AsServer
-- server = RootAPI (pure exampleProducts)

-- app :: Application
-- app = serve (Proxy @RinderAPI) server

-- exampleProducts :: [Product]
-- exampleProducts =
--   [ Product "Milk",
--     Product "Eggs"
--   ]
