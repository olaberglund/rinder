module Store.Grocery where

import Data.Text
import Servant.Client.Core qualified as Core

data Product = Product
    { productId :: !Int
    , productName :: !String
    , productPrice :: !Double
    , productDescription :: !Text
    , productImageUrl :: !Text
    , productOffer :: !Text
    }

data Grocery = Grocery
    { getOffers :: !((Either Core.ClientError [Product]))
    , searchProduct :: !(Text -> IO (Either Core.ClientError [Product]))
    }
