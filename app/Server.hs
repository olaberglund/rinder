module Server where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, intersection, toList)
import Data.Set qualified as Set
import Data.Text hiding (drop, head, map, tail)
import Data.Text qualified as Text hiding (drop, head, map, tail)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Local qualified
import Lucid
import Lucid.Htmx (useHtmx)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hLocation)
import Recipe (Recipe (Recipe), RecipeForm (unvalidatedIngredients, unvalidatedName))
import Servant
import Servant.Client
import Servant.Client.Core qualified as Core
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Willys (Product (Product), Promotion)
import Willys qualified

data RootApi as = RootAPI
  { homePage :: as :- Get '[HTML] HomePage,
    recipePage :: as :- "recept" :> Get '[HTML] RecipePage,
    static :: as :- "static" :> Raw,
    addRecipe :: as :- "add-recipe" :> ReqBody '[FormUrlEncoded] RecipeForm :> PostNoContent
  }
  deriving (Generic)

type Api = NamedRoutes RootApi

app :: Env a -> Application
app = serve (Proxy @Api) . server

server :: Env a -> RootApi AsServer
server env = RootAPI (homePageHandler env) (recipePageHandler env) (serveDirectoryWebApp "static") (newRecipeHandler env)

newRecipeHandler :: Env a -> RecipeForm -> Handler NoContent
newRecipeHandler env recipeForm =
  liftIO (runClientDefault env.manager env.baseUrl env.fetchProducts)
    >>= \case
      Left _err -> throwError err500
      Right products -> do
        liftIO $ TIO.appendFile "recipes.txt" $ formatRecipe (mkRecipe recipeForm products)
        throwError err303 {errHeaders = [(hLocation, "/recept")]}
  where
    formatRecipe :: Recipe -> Text
    formatRecipe (Recipe name ingredients) = name <> "\n" <> (Text.unlines $ map Willys.name $ toList ingredients) <> "\n"

    mkRecipe :: RecipeForm -> Set Product -> Recipe
    mkRecipe rcpf products =
      let maybeIngredients = Set.map Product (unvalidatedIngredients rcpf)
       in Recipe (rcpf.unvalidatedName) (maybeIngredients `intersection` products)

recipePageHandler :: Env a -> Handler RecipePage
recipePageHandler env = do
  recipes <- liftIO $ do
    rawText <- TIO.readFile "recipes.txt"
    let recipes = map parseRecipe $ Text.splitOn "\n\n" rawText
    return (catMaybes recipes)
  liftIO (runClientDefault env.manager env.baseUrl env.fetchProducts) >>= \case
    Right products -> return $ RecipePage (toList products) recipes
    Left _err -> return (RecipePage [] [])
  where
    parseRecipe :: Text -> Maybe Recipe
    parseRecipe raw = Recipe <$> (fst <$> components) <*> (fromList . map Product . snd <$> components)
      where
        components = case Text.lines raw of
          (name : ingrds) -> Just (name, ingrds)
          _ -> Nothing

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
    toHtml Navbar
    h1_ "Välkommen till Olas sida"
    mapM_ toHtml promotions

  toHtmlRaw = toHtml

data RecipePage = RecipePage [Product] [Recipe]

instance ToHtml RecipePage where
  toHtml (RecipePage products recipes) = baseTemplate $ do
    toHtml Navbar
    h1_ "Recept"
    p_ "Här kan du lägga till och se dina recept som används för att matcha mot veckans erbjudanden på Willys."
    i_ "Tips: om en ingrediens kan bytas ut mot en annan, lägg till båda."
    h2_ "Lägg till recept"
    toHtml (RecipeFormComponent products)
    h2_ "Dina recept"
    mapM_ toHtml recipes

  toHtmlRaw = toHtml

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = do
  doctype_
  html_ $ do
    head_ $ do
      useHtmx
      link_ [rel_ "stylesheet", href_ ("static/styles.css")]
      link_ [rel_ "icon", type_ "image/png", href_ "static/images/favicon.ico"]
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      script_ [defer_ "true", type_ "text/javascript", src_ "static/scripts.js"] ("" :: Text)
      title_ "Olas page"
    body_ content
  where

data Navbar = Navbar

instance ToHtml Navbar where
  toHtml Navbar =
    nav_ $
      ul_ $ do
        mapM_
          ( \(href, title) ->
              li_ (a_ [href_ href] $ toHtml title)
          )
          navbarHrefs
    where
      navbarHrefs :: [(Text, Text)]
      navbarHrefs = [("/", "Veckans Erbjudanden"), ("recept", "Mina Recept")]
  toHtmlRaw = toHtml

data RecipeFormComponent = RecipeFormComponent [Product]
  deriving (Generic, Show)

instance ToHtml RecipeFormComponent where
  toHtml (RecipeFormComponent ingredients) = do
    form_ [action_ "/add-recipe", method_ "POST"] $ do
      div_ [class_ "form-group"] $ do
        label_ [for_ "recipe-name"] "Namn:"
        input_ [placeholder_ "Pelles Pitepalt", id_ "recipe-name", name_ "name", type_ "text"]
      div_ [class_ "form-group"] $ do
        button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
        label_ [for_ "chosen-product"] "Ingrediens:"
        input_ [placeholder_ "Ange en ingrediens...", id_ "chosen-product", list_ "products", name_ "chosen-product", type_ "text", autocomplete_ "off"]
        button_ [id_ "add-button", type_ "button", onclick_ "onAddIngredient()"] "Lägg till"
      datalist_ [id_ "products"] $
        mapM_ (option_ . toHtml) ingredients
      label_ [for_ "recipe-ingredients"] "Dina ingredienser:"
      textarea_ [id_ "recipe-ingredients", name_ "ingredients", rows_ "8", readonly_ "true"] ""
      div_ [class_ "form-group"] $ do
        button_ [type_ "button", onclick_ "onResetList()"] "Återställ"
        button_ [type_ "submit", onclick_ "onSubmit"] "Spara"

  toHtmlRaw = toHtml
