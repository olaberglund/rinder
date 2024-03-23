module Server where

import Control.Concurrent (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, eitherDecodeStrict, encode)
import Data.Aeson.KeyMap (Key, fromList)
import Data.Aeson.Text (encodeToLazyText)
import Data.Binary.Builder (fromByteString, fromLazyByteString)
import Data.ByteString (toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text hiding (drop, filter, head, map, null, replicate, tail)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx (hxDelete_, hxExt_, hxParams_, hxPost_, hxSwap_, hxTarget_, hxVals_, useHtmx, useHtmxExtension)
import Network.HTTP.Types (hLocation)
import Network.Wai.EventSource (ServerEvent (..))
import Network.Wai.Middleware.Cors (simpleCors)
import Recipe (Recipe)
import Servant
import Servant.API.EventStream (EventSource, EventStream)
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import System.Timeout (timeout)
import Web.FormUrlEncoded (FromForm)
import Willys (Promotion, fetchProducts, fetchPromotions, runClientDefault, url)
import Willys qualified
import Prelude hiding (product)

data RootApi as = RootAPI
  { homePageEP :: as :- Get '[HTML] NoContent,
    promotionsPageEP :: as :- "erbjudanden" :> Get '[HTML] PromotionsPage,
    staticEP :: as :- "static" :> Raw,
    recipesEP :: as :- "recept" :> NamedRoutes RecipeApi,
    shoppingEP :: as :- "inkop" :> NamedRoutes ShoppingApi
  }
  deriving (Generic)

newtype Search = Search {query :: Text}
  deriving (Generic)

instance FromForm Search

newtype Products = Products {products :: [Willys.Product]}
  deriving (Generic)

type SessionId = Int

data Env = Env {broadcastChan :: Chan ServerEvent}

newEnv :: IO Env
newEnv = Env <$> newChan

data ShoppingApi as = ShoppingApi
  { shoppingPageEP :: as :- Get '[HTML] ShoppingPage,
    productListEP :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList,
    addProductEP :: as :- "lagg-till" :> ReqBody '[JSON] Willys.Product :> Post '[HTML] [ShoppingItem],
    toggleProductEP :: as :- "toggla" :> ReqBody '[JSON] Willys.Product :> Post '[HTML] NoContent,
    removeCheckedEP :: as :- "ta-bort" :> Delete '[HTML] [ShoppingItem],
    removeAllEP :: as :- "ta-bort-alla" :> Delete '[HTML] [ShoppingItem],
    sseEP :: as :- "sse" :> StreamGet NoFraming EventStream EventSource
  }
  deriving (Generic)

data RecipeApi as = RecipeApi
  { recipePageEP :: as :- Get '[HTML] RecipePage,
    addRecipeEP :: as :- "nytt" :> ReqBody '[FormUrlEncoded] Recipe :> PostNoContent,
    productListEP :: as :- "produkter" :> ReqBody '[FormUrlEncoded] Search :> Post '[HTML] ProductSearchList
  }
  deriving (Generic)

app :: Env -> Application
app = simpleCors . serve (Proxy @(NamedRoutes RootApi)) . server

server :: Env -> RootApi AsServer
server env =
  RootAPI
    { homePageEP = redirect "/inkop",
      promotionsPageEP = promotionsPageH,
      staticEP = serveDirectoryWebApp "static",
      recipesEP =
        RecipeApi
          { recipePageEP = recipePageH,
            addRecipeEP = newRecipeH,
            productListEP = productListH (\p -> [onclick_ $ "addIngredient(" <> params p <> ")"])
          },
      shoppingEP =
        ShoppingApi
          { shoppingPageEP = shoppingPageH,
            productListEP = productListH addToShoppingList,
            addProductEP = addProductH env,
            toggleProductEP = toggleProductH env,
            removeCheckedEP = removeCheckedH env,
            removeAllEP = removeAllH,
            sseEP = sseH env
          }
    }

sseH :: Env -> Handler EventSource
sseH env = liftIO $ do
  chan <- dupChan env.broadcastChan
  return $ S.fromStepT (S.Yield keepAlive (rest chan))
  where
    rest :: Chan ServerEvent -> S.StepT IO ServerEvent
    rest chan = S.Effect $ do
      msg <- timeout (15 * 1000000) (readChan chan)
      return $ case msg of
        Just m -> S.Yield m (rest chan)
        Nothing -> S.Yield keepAlive (rest chan)

    keepAlive :: ServerEvent
    keepAlive = CommentEvent (fromByteString "keep-alive")

encodeToText :: (ToJSON v) => [(Key, v)] -> Text
encodeToText = TL.toStrict . encodeToLazyText . fromList

toggle :: Checkbox -> Checkbox
toggle Checked = Unchecked
toggle Unchecked = Checked

removeAllH :: Handler [ShoppingItem]
removeAllH = liftIO $ LBS.writeFile "shopping-list.json" "[]" >> return []

removeCheckedH :: Env -> Handler [ShoppingItem]
removeCheckedH env = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newItems = filter (\i -> i.check == Unchecked) ps
       in updateAndBroadCast env newItems
    Left err -> print err >> return []

toggleProductH :: Env -> Willys.Product -> Handler NoContent
toggleProductH env product = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newItems = map (\i -> if i.product == product then i {check = toggle i.check} else i) ps
       in updateAndBroadCast env newItems >> return NoContent
    Left err -> print err >> return NoContent

asServerEvent :: (ToHtml a) => [a] -> ServerEvent
asServerEvent = ServerEvent Nothing Nothing . map (fromLazyByteString . renderBS . toHtml)

shoppingPageH :: Handler ShoppingPage
shoppingPageH = liftIO $ do
  fetchedPromotions <- runClientDefault fetchPromotions
  shoppingItems <- eitherDecode <$> LBS.readFile "shopping-list.json"
  case shoppingItems of
    Left err -> putStrLn err >> return (ShoppingPage mempty mempty mempty)
    Right list -> return (ShoppingPage mempty (fromRight mempty fetchedPromotions) (Just list))

addProductH :: Env -> Willys.Product -> Handler [ShoppingItem]
addProductH env product = liftIO $ do
  res <- BS.readFile "shopping-list.json"
  case eitherDecodeStrict res of
    Right ps ->
      let newList = ShoppingItem product Unchecked : ps
       in updateAndBroadCast env newList
    Left err -> print err >> return []

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303 {errHeaders = [(hLocation, url)]}

updateAndBroadCast :: Env -> [ShoppingItem] -> IO [ShoppingItem]
updateAndBroadCast env items =
  LBS.writeFile "shopping-list.json" (encode items)
    >> writeChan (broadcastChan env) (asServerEvent items)
    >> return items

productListH :: (Willys.Product -> [Attribute]) -> Search -> Handler ProductSearchList
productListH attributes search = liftIO $ do
  res <- runClientDefault (fetchProducts search.query)
  case res of
    Left err -> print err >> return (ProductSearchList mempty mempty "Sökresultat" "searched-products")
    Right products -> return $ ProductSearchList attributes products "Sökresultat" "searched-products"

recipePageH :: Handler RecipePage
recipePageH = liftIO $ do
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case RecipePage <$> pure toBeReplaced <*> recipes of
    Left err -> putStrLn err >> return (RecipePage mempty mempty)
    Right page -> return page

promotionsPageH :: Handler PromotionsPage
promotionsPageH = liftIO $ do
  fetchedPromotions <- runClientDefault fetchPromotions
  recipes <- eitherDecode <$> LBS.readFile "recipes.json"
  case fetchedPromotions of
    Left err -> print err >> return (PromotionsPage mempty mempty)
    Right promotions ->
      case PromotionsPage <$> pure promotions <*> recipes of
        Left err -> putStrLn err >> return (PromotionsPage mempty mempty)
        Right page -> return page

newRecipeH :: Recipe -> Handler NoContent
newRecipeH recipe = do
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
      useHtmxExtension "sse"
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
          div_ [class_ "product-details"] $ do
            span_ [class_ "product-name"] $ toHtml p.name
            span_ [class_ "product-promo"] $ toHtml $ Willys.getPrice p
            span_ [class_ "product-save"] $ toHtml $ fromMaybe "" $ Willys.getSavePrice p
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
    h2_ "Din inköpslista"
    div_ [class_ "shopping-list-buttons"] $ do
      button_ [class_ "remove-all-button", type_ "button", hxDelete_ "/inkop/ta-bort-alla", hxTarget_ "#shopping-list", hxSwap_ "outerHTML"] "Ta bort alla"
      button_ [class_ "remove-checked-button", type_ "button", hxDelete_ "/inkop/ta-bort", hxTarget_ "#shopping-list", hxSwap_ "outerHTML"] "Ta bort markerade"
    case shoppingList of
      Nothing -> p_ "Något gick fel..."
      Just list -> toHtml list
  button_
    [ class_ "scroll-to-bottom",
      type_ "button",
      onclick_ "document.querySelector('.shopping-list').scrollIntoView({behavior: 'smooth'});"
    ]
    "Till listan"

addToShoppingList :: Willys.Product -> [Attribute]
addToShoppingList p = [hxPost_ "/inkop/lagg-till", hxTarget_ "#shopping-list", hxSwap_ "outerHTML", hxExt_ "json-enc", hxVals_ (Text.decodeUtf8 $ toStrict $ encode p)]

data ShoppingPage = ShoppingPage ![Willys.Product] ![Promotion] !(Maybe [ShoppingItem])

instance ToHtml ShoppingPage where
  toHtml (ShoppingPage products promotions list) = shoppingPage_ products promotions list

  toHtmlRaw = toHtml

shoppingList_ :: (Monad m) => [ShoppingItem] -> HtmlT m ()
shoppingList_ items = div_ [id_ "shopping-list", hxExt_ "sse", hxSseConnect_ "/inkop/sse", hxSseSwap_ "message"] $ do
  mapM_ shoppingItem_ items

data Checkbox = Checked | Unchecked
  deriving (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

data ShoppingItem = ShoppingItem {product :: Willys.Product, check :: Checkbox}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToHtml ShoppingItem where
  toHtml item = shoppingItem_ item
  toHtmlRaw = toHtml

instance ToHtml [ShoppingItem] where
  toHtml shoppingList = shoppingList_ shoppingList
  toHtmlRaw = toHtml

shoppingItem_ :: (Monad m) => ShoppingItem -> HtmlT m ()
shoppingItem_ item = div_ [class_ "shopping-item", id_ divId] $ do
  img_ [class_ "item-image", src_ item.product.image.url]
  div_ [class_ "item-details"] $ do
    div_ [class_ "item-details-text"] $ do
      span_ [class_ "product-name"] $ toHtml item.product.name
      span_ [class_ "item-price"] $ toHtml $ Willys.getPrice item.product
      span_ [class_ "item-save"] $ toHtml $ fromMaybe "" $ Willys.getSavePrice item.product
    input_ $
      [ class_ "item-checkbox",
        type_ "checkbox",
        id_ (Willys.getId item.product),
        name_ "name",
        value_ item.product.name,
        hxPost_ "/inkop/toggla",
        hxExt_ "json-enc",
        hxVals_ (TL.toStrict $ encodeToLazyText item.product),
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

hxSseConnect_ :: Text -> Attribute
hxSseConnect_ = makeAttribute "sse-connect"

hxSseSwap_ :: Text -> Attribute
hxSseSwap_ = makeAttribute "sse-swap"
