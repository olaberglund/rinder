module Main (main) where

import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Server (app, newEnv)
import System.Environment (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        [p] -> case readMay p of
            Nothing -> putStrLn "Port must be an integer"
            Just port ->
                newEnv
                    "transactions.json"
                    "shopping-list.json"
                    >>= run port . app
        _ ->
            newEnv
                "dev-shopping-list.json"
                "dev-transactions.json"
                >>= run 8080 . app
