module Main where

import Network.Wai.Handler.Warp (run)
import Server (app, newEnv)

main :: IO ()
main = do
  p <- read <$> readFile "config.txt"
  newEnv >>= run p . app
