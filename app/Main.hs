module Main where

import Network.Wai.Handler.Warp (run)
import Server (app)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: IO ()
main = do
  -- env <- productionEnv
  run 8080 app
