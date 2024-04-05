module Main (main) where

-- import Lexicon qualified
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Server.App (app)
import Server.Env (newEnv)
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
                "dev-transactions.json"
                "dev-shopping-list.json"
                >>= run 8080 . app
