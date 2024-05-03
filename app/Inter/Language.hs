module Inter.Language (Language (..), mkHref, mkApiHref, toHref, flag, urlPiece) where

import           Data.Text (Text)
import           Servant   (FromHttpApiData (parseUrlPiece))

data Language = SE | EN | Simlish
    deriving stock (Show, Eq, Enum, Bounded)

instance FromHttpApiData Language where
    parseUrlPiece "se"  = Right SE
    parseUrlPiece "en"  = Right EN
    parseUrlPiece "sim" = Right Simlish
    parseUrlPiece _     = Left "Invalid language"

mkHref :: Language -> Text -> Text
mkHref lang path = toHref lang <> path

mkApiHref :: Text -> Text
mkApiHref path = toHref SE <> path

toHref :: Language -> Text
toHref = ("/" <>) . urlPiece

flag :: Language -> Text
flag = \case
    SE -> "🇸🇪"
    EN -> "🇺🇸"
    Simlish -> "💎"

urlPiece :: Language -> Text
urlPiece = \case
    SE -> "se"
    EN -> "en"
    Simlish -> "sim"
