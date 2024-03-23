module Main where

import Network.Wai.Handler.Warp (run)
import Server (app, newEnv)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: IO ()
main = newEnv >>= run 1234 . app
