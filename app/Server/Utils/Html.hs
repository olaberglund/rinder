module Server.Utils.Html (baseTemplate, text, baseTemplate', navbar_, Page404 (..)) where

import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Inter.Language (Language, flag, toHref)
import           Lucid

default (Text)

-- | helper function to convert a Showable to a Text
text :: (Show a) => a -> Text
text = Text.pack . show

baseTemplate :: (Monad m) => Language -> HtmlT m b -> HtmlT m b
baseTemplate lang content = baseTemplate' (navbar_ lang >> content)

baseTemplate' :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate' content = do
    doctype_
    html_ $ do
        head_ $ do
            script_ [src_ "https://unpkg.com/htmx.org@2.0.0/dist/htmx.min.js"] ("" :: Text)
            script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.1/sse.js"] ("" :: Text)
            script_ [src_ "https://unpkg.com/htmx-ext-json-enc@2.0.0/json-enc.js"] ("" :: Text)
            link_ [rel_ "stylesheet", href_ "/styles.css"]
            link_
                [ rel_ "icon"
                , type_ "image/x-icon"
                , href_ "/images/favicon.ico"
                ]
            meta_ [charset_ "utf-8"]
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1"
                ]
            script_
                [ defer_ "true"
                , type_ "text/javascript"
                , src_ "/scripts.js"
                ]
                ("" :: Text)
            title_ "Hörlund"
        body_ $ do
            content

navbar_ :: (Monad m) => Language -> HtmlT m ()
navbar_ _lang = nav_ $ ul_ $ do
    span_ [id_ "language-links"] $ do
        mapM_
            (\(lang', flag') -> li_ (a_ [class_ "language-link", href_ (toHref lang')] flag'))
            (zip (enumFrom minBound) (map (toHtml . flag) (enumFrom minBound)))

-- li_
--     ( a_
--         [href_ $ mkHref lang "/inkop/willys"]
--         (l_ lang Lexicon.NavbarShoppingList)
--     )
-- li_
--     ( a_
--         [href_ $ mkHref lang "/split"]
--         (l_ lang Lexicon.NavbarSplit)
--     )

data Page404 = Page404 !Language !Text

instance ToHtml Page404 where
    toHtml (Page404 lang mtext) = baseTemplate lang $ do
        h1_ "404"
        p_ $ toHtml mtext
    toHtmlRaw = toHtml
