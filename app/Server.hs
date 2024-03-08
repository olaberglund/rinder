{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Server where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Set (toList)
import Data.Text
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Local qualified
import Lucid
import Lucid.Htmx (useHtmx)
import Lucid.Hyperscript (useHyperscript, __)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Client
import Servant.Client.Core qualified as Core
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Willys (Product, Promotion)
import Willys qualified

type RecipePageHref = "recept"

data RootApi as = RootAPI
  { homePage :: as :- Get '[HTML] HomePage,
    recipePage :: as :- RecipePageHref :> Get '[HTML] RecipePage
  }
  deriving (Generic)

type Api = NamedRoutes RootApi

app :: Env a -> Application
app = serve (Proxy @Api) . server

server :: Env a -> RootApi AsServer
server = RootAPI <$> homePageHandler <*> recipePageHandler

recipePageHandler :: Env a -> Handler RecipePage
recipePageHandler env =
  liftIO (runClientDefault env.manager env.baseUrl env.fetchProducts) >>= \case
    Right products -> return $ RecipePage (toList products)
    Left _err -> return (RecipePage [])

homePageHandler :: Env a -> Handler HomePage
homePageHandler env =
  liftIO (runClientDefault env.manager env.baseUrl env.fetchPromotions) >>= \case
    Right promos -> return $ HomePage (toList promos)
    Left _err -> return (HomePage [])

runClientDefault :: Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
runClientDefault mgr url action = runClientM action (addUserAgent $ mkClientEnv mgr url)

productionEnv :: IO (Env Production)
productionEnv = do
  mgr <- newTlsManager
  url <- parseBaseUrl "https://www.willys.se"
  return $ Env mgr url Willys.fetchProducts Willys.fetchPromotions

localEnv :: IO (Env Local)
localEnv = do
  mgr <- newManager defaultManagerSettings
  url <- parseBaseUrl $ "localhost:" <> show Local.port
  return $ Env mgr url Local.fetchProducts Local.fetchPromotions

addUserAgent :: ClientEnv -> ClientEnv
addUserAgent env = env {makeClientRequest = \b -> defaultMakeClientRequest b . Core.addHeader "User-Agent" userAgent}
  where
    userAgent :: Text
    userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

newtype HomePage = HomePage [Promotion]

instance ToHtml HomePage where
  toHtml (HomePage promotions) = baseTemplate $ do
    toHtml (Navbar "/")
    h1_ "Välkommen till Olas sida"
    mapM_ toHtml promotions

  toHtmlRaw = toHtml

newtype RecipePage = RecipePage [Product]

instance ToHtml RecipePage where
  toHtml (RecipePage products) = baseTemplate $ do
    toHtml (Navbar (showHref @RecipePageHref))
    h1_ "Skapa ett recept"
    toHtml (RecipeForm products)

  toHtmlRaw = toHtml

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = do
  doctype_
  html_ $ do
    head_ $ do
      useHtmx
      useHyperscript
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      title_ "Olas page"
    body_ content
  where

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
      navbarHrefs = [("/", "Veckans Erbjudanden"), ("recept", "Mina Recept")]
  toHtmlRaw = toHtml

css_ :: Text -> Attribute
css_ cs = classes_ (splitOn " " cs)

showHref :: forall s. (KnownSymbol s) => Text
showHref = pack (symbolVal (Proxy @s))

data RecipeForm = RecipeForm [Product]
  deriving (Generic)

instance ToHtml RecipeForm where
  toHtml (RecipeForm ingredients) = div_ $ do
    form_
      [ method_ "post",
        css_ "p-4 flex flex-col"
      ]
      $ do
        label_ [for_ "products"] "Välj produkter"
        input_ [id_ "chosen-product", list_ "products", name_ "product", type_ "text"]
        datalist_ [id_ "products"] $ do
          mapM_ (option_ . toHtml) ingredients
        button_ [type_ "button", addIngredientToList] "Lägg till"
    ul_ [id_ "recipe-ingredients"] mempty

  toHtmlRaw = toHtml

addIngredientToList :: Attribute
addIngredientToList =
  [__|
    call document.createElement('li')
    put the (value of the previous <input/>) into its textContent
    put it into #recipe-ingredients
  |]
