module App where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Set (Set)
import Network.HTTP.Client (Manager)
import Servant.Client (BaseUrl, ClientM)
import Willys (Product, Promotion)
import Prelude hiding (putStr)

data DevEnv = Local | Production

data Env (a :: DevEnv) = Env
  { manager :: !Manager,
    baseUrl :: !BaseUrl,
    fetchProducts :: !(ClientM (Set Product)),
    fetchPromotions :: !(ClientM (Set Promotion))
  }

type AppM (a :: DevEnv) = ReaderT (Env a) IO

runApp :: r -> ReaderT r m a -> m a
runApp env m = runReaderT m env
