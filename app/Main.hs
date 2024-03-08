module Main where

import Control.Concurrent (forkIO)
import Local qualified
import Network.Wai.Handler.Warp (run)
import Server (app, localEnv)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: IO ()
main = do
  env <- localEnv
  -- env <- productionEnv
  _ <- forkIO (run Local.port Local.app)
  run 8080 (app env)
