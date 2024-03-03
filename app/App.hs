module App where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl)
import Prelude hiding (putStr)

data Env = Env
  { manager :: !Manager,
    baseUrl :: !BaseUrl,
    port :: !Int
  }

type AppM = ReaderT Env IO

runApp :: r -> ReaderT r m a -> m a
runApp env m = runReaderT m env
