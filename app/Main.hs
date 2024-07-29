module Main (main) where

import           Network.Wai.Handler.Warp (run)
import           Safe                     (readMay)
import           Server.App               (app)
import           Server.Env               (newEnv)
import           System.Environment       (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        ["-h"] -> putStrLn helpMessage
        ["--help"] -> putStrLn helpMessage
        [p, transactionFile, shoppingFile] -> case readMay p of
            Nothing -> putStrLn "Port must be an integer"
            Just port ->
                newEnv
                    transactionFile
                    shoppingFile
                    >>= run port . app
        _ -> putStrLn helpMessage

dev :: IO ()
dev =
    newEnv
        "dev-transactions.json"
        "dev-shopping-list.json"
        >>= run 8080 . app

helpMessage :: String
helpMessage = "Usage: server [port] [transactionFile] [shoppingFile]"
