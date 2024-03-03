module Html where

import Lucid
import Willys (Product)

newtype HomePage = HomePage [Product]

instance ToHtml HomePage where
  toHtml (HomePage products) = baseTemplate $ do
    h1_ "Hello, world!"
    p_ "This is a paragraph."
    p_ "Check out these products:"
    ul_ $ mapM_ (li_ . toHtml) products

  toHtmlRaw = toHtml

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Olas page"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
    body_ content
