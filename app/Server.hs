module Server where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode, eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text hiding (drop, head, map, null, replicate, tail)
import Data.Text qualified as Text hiding (replicate)
import GHC.Generics (Generic)
import Lucid
import Lucid.Htmx (hxParams_, hxPost_, hxSwap_, hxTarget_, useHtmx)
import Network.HTTP.Types (hLocation)
import Recipe (Recipe)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Web.FormUrlEncoded (FromForm)
import Willys (ImageUrl (ImageUrl), Promotion, fetchProducts, fetchPromotions, runClientDefault, url)
import Willys qualified

data RootApi as = RootAPI
  { homePage :: as :- Get '[HTML] HomePage,
    static :: as :- "static" :> Raw,
    recipes :: as :- "recept" :> NamedRoutes RecipeApi,
    shopping :: as :- "inkop" :> NamedRoutes ShoppingApi
  }
  deriving (Generic)

newtype Search = Search {query :: Text}
  deriving (Generic)

instance FromForm Search

data ShoppingApi as = ShoppingApi
  { shoppingPage :: as :- Get '[HTML] ShoppingPage,
    productList :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList
  }
  deriving (Generic)

data RecipeApi as = RecipeApi
  { recipePage :: as :- Get '[HTML] RecipePage,
    addRecipe :: as :- "nytt" :> ReqBody '[FormUrlEncoded] Recipe :> PostNoContent,
    productList :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList
  }
  deriving (Generic)

app :: Application
app = serve (Proxy @(NamedRoutes RootApi)) server

server :: RootApi AsServer
server =
  RootAPI
    homePageHandler
    (serveDirectoryWebApp "static")
    ( RecipeApi
        recipePageHandler
        newRecipeHandler
        (productListHandler (\p -> [onclick_ $ "addIngredient(" <> params p <> ")"]))
    )
    ( ShoppingApi
        (pure $ ShoppingPage [])
        (productListHandler (\p -> []))
    )

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303 {errHeaders = [(hLocation, url)]}

fetch :: (Ord a, FromJSON a) => FilePath -> IO [a]
fetch path = do
  res <- BS.readFile path
  case eitherDecodeStrict res of
    Right (Willys.Response ps _) -> return ps
    Left err -> liftIO (print err) >> return []

productListHandler :: (Willys.Product -> [Attribute]) -> Search -> Handler ProductSearchList
productListHandler attributes search = liftIO $ do
  res <- runClientDefault (fetchProducts search.query)
  case res of
    Left err -> print err >> return (ProductSearchList (const []) [] "Urval")
    Right products -> return $ ProductSearchList attributes products "Urval"

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
    addRecipe :: [Recipe] -> [Recipe]
    addRecipe oldRecipes = recipe : oldRecipes

data HomePage = HomePage [Promotion] [Recipe]

instance ToHtml HomePage where
  toHtml (HomePage promotions recipes) = baseTemplate $ do
    h1_ "Rinder - Tinder för recept"
    h2_ "Matchande recept"
    -- ul_ $ do
    --   let suggestions = recipeSuggestions recipes promotions 1
    --   if Set.null suggestions
    --     then "Inga matchande recept hittades"
    --     else mapM_ (li_ . toHtml) $ suggestions
    h2_ "Veckans erbjudanden (Willys, Lund)"
    div_ [id_ "promotions"] $
      mapM_
        ( \p -> div_ [class_ "product-container", title_ p.product.name] $ do
            img_ [class_ "product", src_ p.product.image.url]
            span_ [class_ "product-name"] $ toHtml p.product.name
        )
        promotions

  toHtmlRaw = toHtml

data RecipePage = RecipePage [Willys.Product] [Recipe]

instance ToHtml RecipePage where
  toHtml (RecipePage products recipes) = baseTemplate $ do
    h1_ "Recept"
    p_ "Här kan du lägga till och se dina recept som används för att matcha mot veckans erbjudanden på Willys."
    i_ "Tips: om en ingrediens kan bytas ut mot en annan, lägg till båda."
    h2_ "Lägg till recept"
    recipeForm_ products
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
    body_ $ do
      navbar_
      content
  where

navbar_ :: (Monad m) => HtmlT m ()
navbar_ = nav_ $ ul_ $ do
  li_ (a_ [href_ "/"] "Erbjudanden")
  li_ (a_ [href_ "/recept"] "Recept")
  li_ (a_ [href_ "/inkop"] "Inköpslista")

data ProductSearchList = ProductSearchList (Willys.Product -> [Attribute]) [Willys.Product] Text
  deriving (Generic)

instance ToHtml ProductSearchList where
  toHtml (ProductSearchList attributes products rubric) = do
    fieldset_ [id_ "products"] $ do
      legend_ (toHtml rubric)
      mapM_
        ( \p -> div_ ([class_ "product-container", title_ p.name] <> (attributes p)) $ do
            img_ [class_ "product", src_ p.image.url]
            span_ [class_ "product-name"] $ toHtml p.name
        )
        products
    where

  toHtmlRaw = toHtml

recipeForm_ :: (Monad m) => [Willys.Product] -> HtmlT m ()
recipeForm_ products = do
  form_ [action_ "/recept/nytt", method_ "POST", id_ "recipe-form"] $ do
    div_ [class_ "form-group"] $ do
      label_ [for_ "recipe-name"] "Namn:"
      input_ [placeholder_ "Pelles Pitepalt", id_ "recipe-name", name_ "name", type_ "text"]
    productSearch_ (const []) products
    label_ [for_ "recipe-ingredients"] "Dina ingredienser:"
    textarea_ [id_ "recipe-ingredients", name_ "ingredients-show", rows_ "8", readonly_ "true"] ""
    div_ [class_ "form-group"] $ do
      button_ [type_ "button", onclick_ "onResetList()"] "Återställ"
      button_ [type_ "submit", onclick_ "onSubmit"] "Spara"

data ShoppingPage = ShoppingPage ![Willys.Product]

instance ToHtml ShoppingPage where
  toHtml (ShoppingPage products) = baseTemplate $ do
    main_ [class_ "shopping-page"] $ do
      h1_ "Veckans inköpslista"
      span_ "Sök och lägg till produkter till din inköpslista."
      form_ $
        productSearch_ (const []) products
      h2_ "Din inköpslista"
      button_ [class_ "remove-checked-button", type_ "button", onclick_ "onDeleteCheckedItems()"] "Ta bort markerade"
      shoppingList_ []

  toHtmlRaw = toHtml

shoppingList_ :: (Monad m) => [Willys.Product] -> HtmlT m ()
shoppingList_ products = div_ [id_ "shopping-list"] $ do
  mapM_ shoppingItem_ exampleProducts
  where
    exampleProducts = map (\(i, p) -> p {Willys.name = p.name <> (Text.pack $ show i)}) $ Prelude.zip [1 ..] $ replicate 10 (Willys.Product "Kyckling" (ImageUrl "https://assets.axfood.se/image/upload/f_auto,t_200/07340083488467_C1N1_s02"))

shoppingItem_ :: (Monad m) => Willys.Product -> HtmlT m ()
shoppingItem_ product = div_ [class_ "shopping-item", id_ ("shopping-item-" <> product.name), onclick_ ("toggleItem('" <> product.name <> "')")] $ do
  img_ [class_ "item-image", src_ product.image.url]
  div_ [class_ "item-details"] $ do
    span_ [class_ "product-name"] $ toHtml product.name
    span_ [class_ "item-quantity"] $ "Mängd: 10"
    input_ [class_ "item-checkbox", type_ "checkbox", id_ product.name, name_ product.name]

productSearch_ :: (Monad m) => (Willys.Product -> [Attribute]) -> [Willys.Product] -> HtmlT m ()
productSearch_ attributes products = do
  div_ [class_ "form-group"] $ do
    button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
    label_ [for_ "query"] "Produkt:"
    input_ [placeholder_ "Sök efter en produkt...", id_ "query", list_ "products", name_ "query", type_ "text", autocomplete_ "off"]
    button_ [id_ "search-button", type_ "button", hxPost_ "/recept/produkter", hxTarget_ "#products", hxSwap_ "outerHTML", hxParams_ "query"] "Visa"
  toHtml (ProductSearchList attributes products "Urval")

data OnClick a = OnClick (a -> Text)

params :: Willys.Product -> Text
params p = "'" <> p.name <> "', '" <> p.image.url <> "'"
