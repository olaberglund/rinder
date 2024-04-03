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
