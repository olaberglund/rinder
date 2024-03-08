module App where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientM)
import Willys (Product, Promotion)
import Prelude hiding (putStr)

data DevEnv = Local | Production

data Env (a :: DevEnv) = Env
  { manager :: !Manager,
    baseUrl :: !BaseUrl,
    fetchProducts :: !(ClientM [Product]),
    fetchPromotions :: !(ClientM [Promotion])
  }

type AppM (a :: DevEnv) = ReaderT (Env a) IO

runApp :: r -> ReaderT r m a -> m a
runApp env m = runReaderT m env
