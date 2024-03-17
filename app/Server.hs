module Server where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, eitherDecodeStrict, encode)
import Data.Aeson.KeyMap (Key, KeyMap, fromList)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Text hiding (drop, filter, head, map, null, replicate, tail)
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Lucid
import Lucid.Htmx (hxDelete_, hxExt_, hxInclude_, hxParams_, hxPost_, hxSwap_, hxTarget_, hxVals_, useHtmx, useHtmxExtension)
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
  { homePage :: as :- Get '[HTML] NoContent,
    promotionsPage :: as :- "erbjudanden" :> Get '[HTML] PromotionsPage,
    static :: as :- "static" :> Raw,
    recipes :: as :- "recept" :> NamedRoutes RecipeApi,
    shopping :: as :- "inkop" :> NamedRoutes ShoppingApi
  }
  deriving (Generic)

newtype Search = Search {query :: Text}
  deriving (Generic)

instance FromForm Search

newtype Products = Products {products :: [Willys.Product]}
  deriving (Generic)

data ShoppingApi as = ShoppingApi
  { shoppingPage :: as :- Get '[HTML] ShoppingPage,
    productList :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList,
    addProduct :: as :- "lagg-till" :> ReqBody '[FormUrlEncoded] Willys.Product :> Post '[HTML] [ShoppingItem],
    toggleProduct :: as :- "toggla" :> ReqBody '[JSON] Willys.Product :> Post '[HTML] [ShoppingItem],
    removeChecked :: as :- "ta-bort" :> Delete '[HTML] [ShoppingItem],
    removeAll :: as :- "ta-bort-alla" :> Delete '[HTML] [ShoppingItem]
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
    (redirect "/inkop")
    promotionsPageHandler
    (serveDirectoryWebApp "static")
    ( RecipeApi
        recipePageHandler
        newRecipeHandler
        (productListHandler (\p -> [onclick_ $ "addIngredient(" <> params p <> ")"]))
    )
    ( ShoppingApi
        shoppingPageHandler
        (productListHandler addToShoppingList)
        addProductHandler
        toggleProductHandler
        removeCheckedHandler
        removeAllHandler
    )

encodeToText :: (ToJSON v) => [(Key, v)] -> Text
encodeToText = TL.toStrict . encodeToLazyText . fromList

toggle :: Checkbox -> Checkbox
toggle Checked = Unchecked
toggle Unchecked = Checked

removeAllHandler :: Handler [ShoppingItem]
removeAllHandler = liftIO $ LBS.writeFile "shopping-list.json" "[]" >> return []

removeCheckedHandler :: Handler [ShoppingItem]
removeCheckedHandler = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newItems = filter (\i -> i.check == Unchecked) ps
       in LBS.writeFile "shopping-list.json" (encode newItems) >> return newItems
    Left err -> print err >> return []

toggleProductHandler :: Willys.Product -> Handler [ShoppingItem]
toggleProductHandler product = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newItems = map (\i -> if i.product == product then i {check = toggle i.check} else i) ps
       in LBS.writeFile "shopping-list.json" (encode newItems) >> return newItems
    Left err -> print err >> return []

shoppingPageHandler :: Handler ShoppingPage
shoppingPageHandler = liftIO $ do
  fetchedPromotions <- runClientDefault fetchPromotions
  shoppingItems <- eitherDecode <$> LBS.readFile "shopping-list.json"
  case shoppingItems of
    Left err -> putStrLn err >> return (ShoppingPage [] [] Nothing)
    Right list -> return (ShoppingPage [] (fromRight [] fetchedPromotions) (Just list))

addProductHandler :: Willys.Product -> Handler [ShoppingItem]
addProductHandler product = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newItem = ShoppingItem product Unchecked
       in LBS.writeFile "shopping-list.json" (encode $ newItem : ps) >> return (newItem : ps)
    Left err -> print err >> return []

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303 {errHeaders = [(hLocation, url)]}

productListHandler :: (Willys.Product -> [Attribute]) -> Search -> Handler ProductSearchList
productListHandler attributes search = liftIO $ do
  res <- runClientDefault (fetchProducts search.query)
  case res of
    Left err -> print err >> return (ProductSearchList mempty mempty "Urval" "searched-products")
    Right products -> return $ ProductSearchList attributes products "Urval" "searched-products"

recipePageHandler :: Handler RecipePage
recipePageHandler = liftIO $ do
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case RecipePage <$> pure toBeReplaced <*> recipes of
    Left err -> putStrLn err >> return (RecipePage mempty mempty)
    Right page -> return page

promotionsPageHandler :: Handler PromotionsPage
promotionsPageHandler = liftIO $ do
  fetchedPromotions <- runClientDefault fetchPromotions
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case fetchedPromotions of
    Left err -> print err >> return (PromotionsPage mempty mempty)
    Right promotions ->
      case PromotionsPage <$> pure promotions <*> recipes of
        Left err -> putStrLn err >> return (PromotionsPage mempty mempty)
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

data PromotionsPage = PromotionsPage [Promotion] [Recipe]

instance ToHtml PromotionsPage where
  toHtml (PromotionsPage promotions _recipes) = baseTemplate $ do
    h1_ "Matchande recept"
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
      useHtmxExtension "json-enc"
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
  li_ (a_ [href_ "/inkop"] "Inköpslista")
  li_ (a_ [href_ "/recept"] "Recept")
  li_ (a_ [href_ "/"] "Erbjudanden")

data ProductSearchList = ProductSearchList (Willys.Product -> [Attribute]) [Willys.Product] Text Text
  deriving (Generic)

instance ToHtml ProductSearchList where
  toHtml (ProductSearchList attributes products rubric listId) = productSearchList_ attributes products rubric listId
  toHtmlRaw = toHtml

productSearchList_ :: (Monad m) => (Willys.Product -> [Attribute]) -> [Willys.Product] -> Text -> Text -> HtmlT m ()
productSearchList_ attributes products rubric listId = do
  fieldset_ [class_ "products", id_ listId] $ do
    legend_ (toHtml rubric)
    mapM_
      ( \p -> div_ ([class_ "product-container", title_ p.name] <> (attributes p)) $ do
          img_ [class_ "product", src_ p.image.url]
          span_ [class_ "product-name"] $ toHtml p.name
      )
      products

recipeForm_ :: (Monad m) => [Willys.Product] -> HtmlT m ()
recipeForm_ products = do
  form_ [action_ "/recept/nytt", method_ "POST", class_ "gapped-form"] $ do
    div_ [class_ "form-group"] $ do
      label_ [for_ "recipe-name"] "Namn:"
      input_ [placeholder_ "Pelles Pitepalt", id_ "recipe-name", name_ "name", type_ "text"]
    productSearch_ toBeReplaced "/recept/produkter" products
    label_ [for_ "recipe-ingredients"] "Dina ingredienser:"
    textarea_ [id_ "recipe-ingredients", name_ "ingredients-show", rows_ "8", readonly_ "true"] ""
    div_ [class_ "form-group"] $ do
      button_ [type_ "button", onclick_ "onResetList()"] "Återställ"
      button_ [type_ "submit", onclick_ "onSubmit"] "Spara"

shoppingPage_ :: (Monad m) => [Willys.Product] -> [Promotion] -> Maybe [ShoppingItem] -> HtmlT m ()
shoppingPage_ products promotions shoppingList = baseTemplate $ do
  h1_ "Veckans inköpslista"
  p_ "Sök och lägg till produkter till din inköpslista."
  form_ [class_ "gapped-form"] $
    productSearch_ toBeReplaced "/inkop/produkter" products
  toHtml $ ProductSearchList addToShoppingList (map (.product) promotions) "Erbjudanden" "promotion-products"
  div_ [class_ "shopping-list"] $ do
    div_ [class_ "shopping-list-title"] $ do
      h2_ "Din inköpslista"
      button_ [class_ "remove-all-button", type_ "button", hxDelete_ "/inkop/ta-bort-alla", hxTarget_ "#shopping-list", hxSwap_ "outerHTML"] "Ta bort alla"
      button_ [class_ "remove-checked-button", type_ "button", hxDelete_ "/inkop/ta-bort", hxTarget_ "#shopping-list", hxSwap_ "outerHTML"] "Ta bort markerade"
    case shoppingList of
      Nothing -> p_ "Något gick fel..."
      Just list -> toHtml list

addToShoppingList :: Willys.Product -> [Attribute]
addToShoppingList p = [hxPost_ "/inkop/lagg-till", hxTarget_ "#shopping-list", hxSwap_ "outerHTML", hxVals_ (encodeToText [("name", p.name), ("url", p.image.url)])]

data ShoppingPage = ShoppingPage ![Willys.Product] ![Promotion] !(Maybe [ShoppingItem])

instance ToHtml ShoppingPage where
  toHtml (ShoppingPage products promotions list) = shoppingPage_ products promotions list

  toHtmlRaw = toHtml

shoppingList_ :: (Monad m) => [ShoppingItem] -> HtmlT m ()
shoppingList_ items = div_ [id_ "shopping-list"] $ do
  mapM_ shoppingItem_ items

data Checkbox = Checked | Unchecked
  deriving (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data ShoppingItem = ShoppingItem {product :: Willys.Product, check :: Checkbox}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToHtml [ShoppingItem] where
  toHtml shoppingList = shoppingList_ shoppingList
  toHtmlRaw = toHtml

shoppingItem_ :: (Monad m) => ShoppingItem -> HtmlT m ()
shoppingItem_ item = div_ [class_ "shopping-item", id_ divId] $ do
  img_ [class_ "item-image", src_ item.product.image.url]
  div_ [class_ "item-details"] $ do
    div_ [class_ "item-details-text"] $ do
      span_ [class_ "product-name"] $ toHtml item.product.name
      span_ [class_ "item-quantity"] $ ""
    input_ $
      [ class_ "item-checkbox",
        type_ "checkbox",
        id_ $ Willys.getId item.product,
        name_ "name",
        value_ item.product.name,
        hxPost_ "/inkop/toggla",
        hxTarget_ "#shopping-list",
        hxExt_ "json-enc",
        hxVals_ (TL.toStrict $ encodeToLazyText item.product),
        hxSwap_ "outerHTML",
        autocomplete_ "off"
      ]
        <> if item.check == Checked then [checked_] else []
  where
    divId = "shopping-item-" <> Willys.getId item.product

productSearch_ :: (Monad m) => (Willys.Product -> [Attribute]) -> Text -> [Willys.Product] -> HtmlT m ()
productSearch_ attributes posturl products = do
  div_ [class_ "form-group"] $ do
    button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
    label_ [for_ "query"] "Produkt:"
    input_ [placeholder_ "Sök efter en produkt...", id_ "query", list_ "products", name_ "query", type_ "text", autocomplete_ "off"]
    button_ [id_ "search-button", type_ "button", hxPost_ posturl, hxTarget_ ("#" <> listId), hxSwap_ "outerHTML", hxParams_ "query"] "Visa"
  toHtml (ProductSearchList attributes products "Sökresultat" listId)
  where
    listId = "searched-products"

data OnClick a = OnClick (a -> Text)

params :: Willys.Product -> Text
params p = "'" <> p.name <> "', '" <> p.image.url <> "'"

toBeReplaced :: (Monoid m) => m
toBeReplaced = mempty
