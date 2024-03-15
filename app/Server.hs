module Server where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text hiding (drop, head, map, null, tail)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Lucid
import Lucid.Htmx (hxParams_, hxPost_, hxSwap_, hxTarget_, useHtmx)
import Network.HTTP.Types (hLocation)
import Recipe (Recipe, recipeSuggestions)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Web.FormUrlEncoded (FromForm)
import Willys (Promotion, fetchProducts, fetchPromotions, runClientDefault, url)
import Willys qualified

data RootApi as = RootAPI
  { homePage :: as :- Get '[HTML] HomePage,
    static :: as :- "static" :> Raw,
    recipes :: as :- "recept" :> NamedRoutes RecipeApi
  }
  deriving (Generic)

type Api = NamedRoutes RootApi

newtype Search = Search {query :: Text}
  deriving (Generic)

instance FromForm Search

data RecipeApi as = RecipeApi
  { recipePage :: as :- Get '[HTML] RecipePage,
    addRecipe :: as :- "nytt" :> ReqBody '[FormUrlEncoded] Recipe :> PostNoContent,
    productList :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductList
  }
  deriving (Generic)

app :: Application
app = serve (Proxy @Api) server

server :: RootApi AsServer
server =
  RootAPI
    homePageHandler
    (serveDirectoryWebApp "static")
    ( RecipeApi
        recipePageHandler
        newRecipeHandler
        productListHandler
    )

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303 {errHeaders = [(hLocation, url)]}

fetch :: (Ord a, FromJSON a) => FilePath -> IO (Set a)
fetch path = do
  res <- BS.readFile path
  case eitherDecodeStrict res of
    Right (Willys.Response ps _) -> return ps
    Left err -> liftIO (print err) >> return Set.empty

productListHandler :: Search -> Handler ProductList
productListHandler search = liftIO $ do
  res <- runClientDefault (fetchProducts search.query)
  case res of
    Left err -> print err >> return (ProductList mempty search.query)
    Right products -> return $ ProductList products search.query

recipePageHandler :: Handler RecipePage
recipePageHandler = liftIO $ do
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case RecipePage <$> pure mempty <*> recipes of
    Left err -> putStrLn err >> return (RecipePage mempty mempty)
    Right page -> return page

homePageHandler :: Handler HomePage
homePageHandler = liftIO $ do
  fetchedPromotions <- runClientDefault fetchPromotions
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case fetchedPromotions of
    Left err -> print err >> return (HomePage mempty mempty)
    Right promotions ->
      case HomePage <$> pure promotions <*> recipes of
        Left err -> putStrLn err >> return (HomePage mempty mempty)
        Right page -> return page

newRecipeHandler :: Recipe -> Handler NoContent
newRecipeHandler recipe = do
  liftIO $ do
    oldRecipes <- eitherDecode <$> LBS.readFile "recipes.json"
    case addRecipe <$> oldRecipes of
      Left err -> putStrLn err
      Right recipes -> LBS.writeFile "recipes.json" $ encode recipes

  redirect "/recept"
  where
    addRecipe :: Set Recipe -> Set Recipe
    addRecipe oldRecipes = recipe `Set.insert` oldRecipes

data HomePage = HomePage (Set Promotion) (Set Recipe)

instance ToHtml HomePage where
  toHtml (HomePage promotions recipes) = baseTemplate $ do
    toHtml Navbar
    h1_ "Rinder - Tinder för recept"
    h2_ "Matchande recept"
    ul_ $ do
      let suggestions = recipeSuggestions recipes promotions 1
      if Set.null suggestions
        then "Inga matchande recept hittades"
        else mapM_ (li_ . toHtml) $ suggestions
    h2_ "Veckans erbjudanden (Willys, Lund)"
    div_ [class_ "promotions-container"] $
      mapM_ toHtml promotions

  toHtmlRaw = toHtml

data RecipePage = RecipePage (Set Willys.Product) (Set Recipe)

instance ToHtml RecipePage where
  toHtml (RecipePage products recipes) = baseTemplate $ do
    toHtml Navbar
    h1_ "Recept"
    p_ "Här kan du lägga till och se dina recept som används för att matcha mot veckans erbjudanden på Willys."
    i_ "Tips: om en ingrediens kan bytas ut mot en annan, lägg till båda."
    h2_ "Lägg till recept"
    toHtml (RecipeFormComponent products)
    h2_ "Dina recept"
    ul_ $ do
      mapM_ (li_ . toHtml) recipes

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
      navbarHrefs = [("/", "Veckans Erbjudanden"), ("recept", "Mina Recept"), ("inkop", "Inköpslista")]
  toHtmlRaw = toHtml

data RecipeFormComponent = RecipeFormComponent (Set Willys.Product)
  deriving (Generic, Show)

data ProductList = ProductList (Set Willys.Product) Text
  deriving (Generic, Show)

instance ToHtml ProductList where
  toHtml (ProductList products query) = do
    fieldset_ [id_ "products"] $ do
      legend_ "Urval"
      if Text.null query
        then mempty
        else do
          mapM_
            ( \p -> div_ [onclick_ ("addIngredient(" <> params p <> ")"), class_ "product-container", title_ p.name] $ do
                img_ [class_ "product", src_ p.image.url]
                span_ [class_ "product-name"] $ toHtml p.name
            )
            products
    where
      params p = "'" <> p.name <> "', '" <> p.image.url <> "'"

  toHtmlRaw = toHtml

instance ToHtml RecipeFormComponent where
  toHtml (RecipeFormComponent products) = do
    form_ [action_ "/recept/nytt", method_ "POST", id_ "recipe-form"] $ do
      div_ [class_ "form-group"] $ do
        label_ [for_ "recipe-name"] "Namn:"
        input_ [placeholder_ "Pelles Pitepalt", id_ "recipe-name", name_ "name", type_ "text"]
      div_ [class_ "form-group"] $ do
        button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
        label_ [for_ "query"] "Ingrediens:"
        input_ [placeholder_ "Sök efter en ingrediens...", id_ "query", list_ "products", name_ "query", type_ "text", autocomplete_ "off"]
        button_ [id_ "search-button", type_ "button", hxPost_ "/recept/produkter", hxTarget_ "#products", hxSwap_ "outerHTML", hxParams_ "query"] "Visa"
      toHtml (ProductList products "")
      label_ [for_ "recipe-ingredients"] "Dina ingredienser:"
      textarea_ [id_ "recipe-ingredients", name_ "ingredients-show", rows_ "8", readonly_ "true"] ""
      div_ [class_ "form-group"] $ do
        button_ [type_ "button", onclick_ "onResetList()"] "Återställ"
        button_ [type_ "submit", onclick_ "onSubmit"] "Spara"

  toHtmlRaw = toHtml
