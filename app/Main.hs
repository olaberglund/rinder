module Main where

import Network.Wai.Handler.Warp (run)
import Server (app)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: IO ()
main = run 1234 app
