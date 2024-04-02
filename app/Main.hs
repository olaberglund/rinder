module Main (main) where

import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Server (app, newEnv)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    env <- newEnv
    case args of
        [p] -> case readMay p of
            Nothing -> putStrLn "Port must be an integer"
            Just port -> run port (app env)
        _ -> run 8080 (app env)
