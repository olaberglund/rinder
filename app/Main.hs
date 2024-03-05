module Main where

import Control.Concurrent (forkIO)
import Local qualified
import Network.Wai.Handler.Warp (run)
import Server (app, localEnv)
import Prelude hiding (putStrLn, readFile, writeFile)

promotionsUrl :: String
promotionsUrl = "willys.se/search/campaigns/offline"

productsUrl :: String
productsUrl = "willys.se/c"

main :: IO ()
main = do
  env <- localEnv
  _ <- forkIO (run 8082 Local.app)
  run 8080 (app env)
