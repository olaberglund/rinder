module Server where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, eitherDecodeStrict, encode)
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text hiding (drop, head, map, null, replicate, tail)
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Lucid
import Lucid.Htmx (hxParams_, hxPost_, hxSwap_, hxTarget_, hxVals_, useHtmx)
import Network.HTTP.Types (hLocation)
import Recipe (Recipe)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Web.FormUrlEncoded (FromForm)
import Willys (Promotion, fetchProducts, fetchPromotions, runClientDefault, url)
import Willys qualified
import Prelude hiding (product)

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
    productList :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList,
    addProduct :: as :- "lista" :> ReqBody '[FormUrlEncoded] Willys.Product :> Post '[HTML] ShoppingList
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
        shoppingPageHandler
        (productListHandler (\p -> [hxPost_ "/inkop/lista", hxTarget_ "#shopping-list", hxSwap_ "outerHTML", hxVals_ (encodeToText [("name", p.name), ("url", p.image.url)])]))
        addProductHandler
    )
  where
    encodeToText = TL.toStrict . encodeToLazyText . fromList

shoppingPageHandler :: Handler ShoppingPage
shoppingPageHandler = liftIO $ do
  products <- eitherDecode <$> LBS.readFile "shopping-list.json"
  case products of
    Left err -> putStrLn err >> return (ShoppingPage [] [])
    Right (ShoppingList ps) -> return (ShoppingPage [] ps)

addProductHandler :: Willys.Product -> Handler ShoppingList
addProductHandler product = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps -> LBS.writeFile "shopping-list.json" (encode $ ShoppingList (product : ps)) >> return (ShoppingList (product : ps))
    Left err -> print err >> return (ShoppingList [])

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303 {errHeaders = [(hLocation, url)]}

productListHandler :: (Willys.Product -> [Attribute]) -> Search -> Handler ProductSearchList
productListHandler attributes search = liftIO $ do
  res <- runClientDefault (fetchProducts search.query)
  case res of
    Left err -> print err >> return (ProductSearchList mempty mempty "Urval")
    Right products -> return $ ProductSearchList attributes products "Urval"

recipePageHandler :: Handler RecipePage
recipePageHandler = liftIO $ do
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case RecipePage <$> pure toBeReplaced <*> recipes of
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
  toHtml (HomePage promotions _recipes) = baseTemplate $ do
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
  toHtml (ProductSearchList attributes products rubric) = productSearchList_ attributes products rubric
  toHtmlRaw = toHtml

productSearchList_ :: (Monad m) => (Willys.Product -> [Attribute]) -> [Willys.Product] -> Text -> HtmlT m ()
productSearchList_ attributes products rubric = do
  fieldset_ [id_ "products"] $ do
    legend_ (toHtml rubric)
    mapM_
      ( \p -> div_ ([class_ "product-container", title_ p.name] <> (attributes p)) $ do
          img_ [class_ "product", src_ p.image.url]
          span_ [class_ "product-name"] $ toHtml p.name
      )
      products

recipeForm_ :: (Monad m) => [Willys.Product] -> HtmlT m ()
recipeForm_ products = do
  form_ [action_ "/recept/nytt", method_ "POST", id_ "recipe-form"] $ do
    div_ [class_ "form-group"] $ do
      label_ [for_ "recipe-name"] "Namn:"
      input_ [placeholder_ "Pelles Pitepalt", id_ "recipe-name", name_ "name", type_ "text"]
    productSearch_ toBeReplaced "/recept/produkter" products
    label_ [for_ "recipe-ingredients"] "Dina ingredienser:"
    textarea_ [id_ "recipe-ingredients", name_ "ingredients-show", rows_ "8", readonly_ "true"] ""
    div_ [class_ "form-group"] $ do
      button_ [type_ "button", onclick_ "onResetList()"] "Återställ"
      button_ [type_ "submit", onclick_ "onSubmit"] "Spara"

shoppingPage_ :: (Monad m) => [Willys.Product] -> [Willys.Product] -> HtmlT m ()
shoppingPage_ products shoppingList = baseTemplate $ do
  h1_ "Veckans inköpslista"
  p_ "Sök och lägg till produkter till din inköpslista."
  form_ $
    productSearch_ toBeReplaced "/inkop/produkter" products
  div_ [class_ "shopping-list-title"] $ do
    h2_ "Din inköpslista"
    button_ [class_ "remove-checked-button", type_ "button", onclick_ "onDeleteCheckedItems()"] "Ta bort markerade"
  shoppingList_ shoppingList

data ShoppingPage = ShoppingPage ![Willys.Product] ![Willys.Product]

instance ToHtml ShoppingPage where
  toHtml (ShoppingPage products list) = shoppingPage_ products list

  toHtmlRaw = toHtml

shoppingList_ :: (Monad m) => [Willys.Product] -> HtmlT m ()
shoppingList_ products = div_ [id_ "shopping-list"] $ do
  mapM_ shoppingItem_ products

data ShoppingList = ShoppingList ![Willys.Product]
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToHtml ShoppingList where
  toHtml (ShoppingList products) = shoppingList_ products
  toHtmlRaw = toHtml

shoppingItem_ :: (Monad m) => Willys.Product -> HtmlT m ()
shoppingItem_ product = div_ [class_ "shopping-item", id_ ("shopping-item-" <> product.name), onclick_ ("toggleItem('" <> product.name <> "')")] $ do
  img_ [class_ "item-image", src_ product.image.url]
  div_ [class_ "item-details"] $ do
    span_ [class_ "product-name"] $ toHtml product.name
    span_ [class_ "item-quantity"] $ "Mängd: 10"
    input_ [class_ "item-checkbox", type_ "checkbox", id_ product.name, name_ product.name]

productSearch_ :: (Monad m) => (Willys.Product -> [Attribute]) -> Text -> [Willys.Product] -> HtmlT m ()
productSearch_ attributes posturl products = do
  div_ [class_ "form-group"] $ do
    button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
    label_ [for_ "query"] "Produkt:"
    input_ [placeholder_ "Sök efter en produkt...", id_ "query", list_ "products", name_ "query", type_ "text", autocomplete_ "off"]
    button_ [id_ "search-button", type_ "button", hxPost_ posturl, hxTarget_ "#products", hxSwap_ "outerHTML", hxParams_ "query"] "Visa"
  toHtml (ProductSearchList attributes products "Urval")

data OnClick a = OnClick (a -> Text)

params :: Willys.Product -> Text
params p = "'" <> p.name <> "', '" <> p.image.url <> "'"

toBeReplaced :: (Monoid m) => m
toBeReplaced = mempty
