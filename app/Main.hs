module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client (AsClientT, BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientM, Scheme (Http, Https), client, defaultMakeClientRequest, makeClientRequest, mkClientEnv, parseBaseUrl, runClientM, (//), (/:))
import qualified Servant.Client.Core as Core
import Willys
import Prelude hiding (putStr)

promotionsUrl :: String
promotionsUrl = "https://willys.se/search/campaigns/offline"

productsUrl :: String
productsUrl = "https://willys.se/c"

main :: IO ()
main = do
  manager' <- newTlsManager
  burl <- parseBaseUrl productsUrl
  res <-
    runClientM
      fetchAllProducts
      ((mkClientEnv manager' burl) {makeClientRequest = \b r -> defaultMakeClientRequest b (Core.addHeader "User-Agent" userAgent r)})
  -- res <-
  --   runClientM fetchPromotions (mkClientEnv manager' baseUrl)
  print res

apiClient :: WillysRootAPI (AsClientT ClientM)
apiClient = client (Proxy @WillysAPI)

fetchPromotions :: ClientM [Promotion]
fetchPromotions = apiClient // getPromotions /: Just 2176 /: Just "PERSONAL_GENERAL" /: Just 2000

fetchAllProducts :: ClientM [Product]
fetchAllProducts =
  liftIO (mapConcurrently (return . fetchProduct) productHrefs)
    >>= fmap concat . sequence
  where
    fetchProduct :: Text -> ClientM [Product]
    fetchProduct href = apiClient // getProducts /: href /: Just 10

    productHrefs :: [Text]
    productHrefs =
      [ "kott-chark-och-fagel"
      -- "frukt-och-gront",
      -- "mejeri-ost-och-agg",
      -- "skafferi",
      -- "brod-och-kakor",
      -- "fryst",
      -- "fisk-och-skaldjur",
      -- "vegetariskt"
      ]
