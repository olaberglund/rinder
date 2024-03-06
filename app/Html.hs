module Html where

import Data.Text
import Lucid
import Lucid.Htmx (useHtmx)
import Willys (Promotion)

newtype HomePage = HomePage [Promotion]

instance ToHtml HomePage where
  toHtml (HomePage promotions) = baseTemplate $ do
    toHtml (Navbar "")
    h1_ "Välkommen till Olas sida"
    mapM_ toHtml promotions

  -- ul_ $ mapM_ (li_ . toHtml) products

  toHtmlRaw = toHtml

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = do
  doctype_
  html_ [css_ "bg-black text-white"] $ do
    head_ $ do
      useHtmx
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      title_ "Olas page"
    body_ content

newtype Navbar = Navbar Text

instance ToHtml Navbar where
  toHtml (Navbar currentHref) =
    nav_ [css_ "p-4"] $
      ul_ [css_ "flex justify-center space-x-5"] $ do
        mapM_
          ( \(href, title) ->
              li_ (a_ [href_ href, css_ (if currentHref == href then "underline" else "")] $ toHtml title)
          )
          navbarHrefs
    where
      navbarHrefs :: [(Text, Text)]
      navbarHrefs = [("", "Veckans Erbjudanden"), ("recept", "Mina Recept")]
  toHtmlRaw = toHtml

css_ :: Text -> Attribute
css_ cs = classes_ (splitOn " " cs)
