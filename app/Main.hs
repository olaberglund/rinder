module Main where

import Data.Aeson (Value, decode, eitherDecode, encode)
import Data.ByteString.Lazy.Char8
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (parseBaseUrl)
import Server (runClientDefault)
import Willys (Product, ProductResponse, fetchProducts)
import Prelude hiding (putStrLn, readFile, writeFile)

promotionsUrl :: String
promotionsUrl = "https://willys.se/search/campaigns/offline"

productsUrl :: String
productsUrl = "willys.se/c"

main :: IO ()
main = do
  mgr <- newTlsManager
  url <- parseBaseUrl productsUrl
  -- res <- runClientDefault mgr url fetchProducts
  jsonprods <- readFile "product-category.json"

  -- let res :: Either String ProductResponse = eitherDecode jsonprods
  print 23
