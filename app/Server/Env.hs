{- | This module contains the environment for the server. The environment is a
record that contains the configuration for the server, and it is used to pass
the configuration to the handlers and the server.

The filepath for the transactions file and the shopping list file are stored in
the environment, and varies depending on whether the server is running in
development or production mode. The broadcast channel is used to send messages
to the clients using Server-Sent Events (SSE).
-}
module Server.Env (Env (..), newEnv) where

import Control.Concurrent (Chan, newChan)
import Network.Wai.EventSource (ServerEvent (..))

data Env = Env
    { envTransactionsFile :: !FilePath
    , envShoppingListFile :: !FilePath
    , envBroadcastChan :: !(Chan ServerEvent)
    }

newEnv :: FilePath -> FilePath -> IO Env
newEnv trFile shopFile = Env trFile shopFile <$> newChan
