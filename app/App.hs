module App where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import GHC.Generics (Generic)
import Html (HomePage)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client (BaseUrl, ClientM)
import Servant.HTML.Lucid (HTML)
import Willys (Product, Promotion)
import Prelude hiding (putStr)

data DevEnv = Local | Production

data Env (a :: DevEnv) = Env
  { manager :: !Manager,
    baseUrl :: !BaseUrl,
    fetchProducts :: !(ClientM [Product]),
    fetchPromotions :: !(ClientM [Promotion])
  }

newtype RootApi as = RootAPI
  { getPromotions :: as :- Get '[HTML] HomePage
  }
  deriving (Generic)

type Api = NamedRoutes RootApi

type AppM (a :: DevEnv) = ReaderT (Env a) IO

runApp :: r -> ReaderT r m a -> m a
runApp env m = runReaderT m env
