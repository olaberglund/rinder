module Server.Utils.Handler (err404', hxRedirect, customFormatters, redirect)
where

import Data.ByteString qualified as BS
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Inter.Language (Language)
import Lucid (ToHtml (toHtml), renderBS)
import Network.HTTP.Types (hLocation)
import Servant (
    ErrorFormatters (notFoundErrorFormatter),
    Handler,
    ServerError (errBody, errHeaders),
    err303,
    err404,
    throwError,
 )
import Servant.Server (defaultErrorFormatters)
import Server.Utils.Html

customFormatters :: Language -> ErrorFormatters
customFormatters lang =
    defaultErrorFormatters
        { notFoundErrorFormatter = const (err404' lang Nothing)
        }

err404' :: Language -> Maybe Text -> ServerError
err404' lang msg =
    err404
        { errBody =
            renderBS $
                toHtml
                    (Page404 lang (Maybe.fromMaybe "Inget att se här..." msg))
        }

hxRedirect :: BS.ByteString -> Handler a
hxRedirect url = throwError err303{errHeaders = [("HX-Redirect", url)]}

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303{errHeaders = [(hLocation, url)]}
