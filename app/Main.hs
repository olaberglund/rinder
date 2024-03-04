module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try)
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
  try startServer >>= \case
    Left (e :: IOError) -> print $ "Error starting server: " <> show e
    Right _ -> print "Server"

startServer :: IO ()
startServer = forkIO (run 8082 Local.app) >> localEnv >>= run 8080 . app
