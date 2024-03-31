{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.Char (isNumber)
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (getCurrentTime, getCurrentTimeZone)
import GHC.Generics (Generic)
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx (hxDelete_, hxExt_, hxParams_, hxPost_, hxSwap_, hxTarget_, hxVals_, useHtmx, useHtmxExtension)
import Network.HTTP.Types (hLocation)
import Network.Wai.EventSource (ServerEvent (..))
import Numeric (showFFloat)
import Servant
import Servant.API.EventStream (EventSource, EventStream)
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import Splitvajs
import System.Timeout (timeout)
import Web.FormUrlEncoded (FromForm)
import Willys (Promotion, fetchProducts, fetchPromotions, runClientDefault, safeHead, url)
import Willys qualified
import Prelude hiding (exp, product)

data RootApi as = RootAPI
  { homePageEP :: as :- Get '[HTML] NoContent,
    staticEP :: as :- "static" :> Raw,
    shoppingEP :: as :- "inkop" :> NamedRoutes ShoppingApi,
    splitEP :: as :- "split" :> NamedRoutes SplitApi
  }
  deriving (Generic)

newtype Search = Search {query :: Text}
  deriving (Generic)

instance FromForm Search

newtype Products = Products {products :: [Willys.Product]}
  deriving (Generic)

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

data SplitApi as = SplitApi
  { splitPageEP :: as :- Get '[HTML] SplitPage,
    newExpenseEP :: as :- "lagg-till" :> ReqBody '[FormUrlEncoded] ExpenseForm :> Post '[HTML] Transactions,
    settleUpEP :: as :- "gor-upp" :> Post '[HTML] Transactions,
    editExpensePageEP :: as :- "redigera" :> Capture "date" Text :> Get '[HTML] (Either Page404 EditExpensePage)
  }
  deriving (Generic)

app :: Env -> Application
app = serve (Proxy @(NamedRoutes RootApi)) . server

server :: Env -> RootApi AsServer
server env =
  RootAPI
    { homePageEP = redirect "/split",
      staticEP = serveDirectoryWebApp "static",
      shoppingEP =
        ShoppingApi
          { shoppingPageEP = shoppingPageH,
            productListEP = productListH addToShoppingList,
            addProductEP = addProductH env,
            toggleProductEP = toggleProductH env,
            removeCheckedEP = removeCheckedH env,
            removeAllEP = removeAllH,
            sseEP = sseH env
          },
      splitEP =
        SplitApi
          { splitPageEP = splitPageH,
            newExpenseEP = newExpenseH,
            settleUpEP = settleUpH,
            editExpensePageEP = editExpensePageH
          }
    }

editExpensePageH :: Text -> Handler (Either Page404 EditExpensePage)
editExpensePageH encodedDate = liftIO $ do
  res <- BS.readFile "transactions.json"
  case eitherDecodeStrict res of
    Right (ts :: [Transaction]) -> do
      let expenses = [exp | ExpenseTransaction exp@(Expense {date = d}) <- ts, encodeDate d == encodedDate]
      case safeHead expenses of
        Just (Expense {date = d}) -> return (Right $ EditExpensePage $ formatDate d)
        Nothing -> return (Left (Page404 (Just "Ingen sådan utgift hittades")))
    Left err -> print err >> return (Left (Page404 Nothing))

settleUpH :: Handler Transactions
settleUpH = liftIO $ do
  res <- BS.readFile "transactions.json"
  utc <- getCurrentTime
  tz <- getCurrentTimeZone
  case eitherDecodeStrict res of
    Right ts -> do
      let newTs = map SettlementTransaction (settlements tz utc ts) <> ts
      LBS.writeFile "transactions.json" (encode newTs)
      return (Transactions newTs)
    Left err -> print err >> return (Transactions [])

newExpenseH :: ExpenseForm -> Handler Transactions
newExpenseH form = liftIO $ do
  utc <- getCurrentTime
  tz <- getCurrentTimeZone
  res <- BS.readFile "transactions.json"
  case eitherDecodeStrict res of
    Right ts -> do
      let newTs = ExpenseTransaction (toExpense form tz utc) : ts
      LBS.writeFile "transactions.json" (encode newTs)
      return (Transactions newTs)
    Left err -> print err >> return (Transactions [])

splitPageH :: Handler SplitPage
splitPageH = liftIO $ do
  res <- BS.readFile "transactions.json"
  case eitherDecodeStrict res of
    Right ts -> return (SplitPage ts)
    Left err -> print err >> return (SplitPage [])

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

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = baseTemplate' (navbar_ >> content)

baseTemplate' :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate' content = do
  doctype_
  html_ $ do
    head_ $ do
      useHtmx
      useHtmxExtension "json-enc"
      useHtmxExtension "sse"
      link_ [rel_ "stylesheet", href_ ("/static/styles.css")]
      link_ [rel_ "icon", type_ "image/png", href_ "/static/images/favicon.ico"]
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      script_ [defer_ "true", type_ "text/javascript", src_ "/static/scripts.js"] ("" :: Text)
      title_ "Olas page"
    body_ $ do
      content
  where

data Page404 = Page404 (Maybe Text)

instance ToHtml Page404 where
  toHtml (Page404 mtext) = baseTemplate $ do
    h1_ "404"
    p_ $ toHtml $ fromMaybe "Page not found" mtext
  toHtmlRaw = toHtml

instance (ToHtml a, ToHtml b) => ToHtml (Either a b) where
  toHtml (Left a) = toHtml a
  toHtml (Right b) = toHtml b
  toHtmlRaw = toHtml

navbar_ :: (Monad m) => HtmlT m ()
navbar_ = nav_ $ ul_ $ do
  li_ (a_ [href_ "/inkop"] "Inköpslista")
  li_ (a_ [href_ "/split"] "Split")

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

shoppingPage_ :: (Monad m) => [Willys.Product] -> [Promotion] -> Maybe [ShoppingItem] -> HtmlT m ()
shoppingPage_ products promotions shoppingList = baseTemplate $ do
  h1_ "Veckans inköpslista"
  p_ "Sök och lägg till produkter till inköpslistan."
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
addToShoppingList p = [hxPost_ "/inkop/lagg-till", hxTarget_ "#shopping-list", hxSwap_ "outerHTML", hxExt_ "json-enc", hxVals_ (TE.decodeUtf8 $ toStrict $ encode p)]

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

splitPage_ :: (Monad m) => [Transaction] -> HtmlT m ()
splitPage_ expenses =
  baseTemplate $ do
    h1_ "Splitvajs"
    h2_ "Lägg till en utgift"
    form_ [class_ "gapped-form", id_ "split-form"] $ do
      div_ [class_ "form-group"] $ do
        label_ [for_ "rubric"] "Rubrik:"
        input_ [type_ "text", id_ "rubric", name_ "rubric"]
      fieldset_ [class_ "radio-form-group"] $ do
        legend_ "Betalare"
        mapM_
          ( \p -> div_ [class_ "radio-group"] $ do
              input_ $ [type_ "radio", id_ (name p), name_ "paidBy", value_ (name p)] <> (if p == head people then [checked_] else mempty)
              label_ [for_ (name p)] (toHtml p)
          )
          people
      fieldset_ [class_ "debt-form-group"] $ do
        legend_ "Skuld"
        div_ [class_ "debtor-form-group"] $ do
          select_ [name_ "debtor"] $ do
            mapM_
              ( \p -> option_ [value_ (name p)] (toHtml p)
              )
              (reverse people)
          span_ [class_ "form-comment"] "ska betala"
        div_ [class_ "debtor-form-group"] $ do
          input_ [type_ "number", id_ "amount", name_ "amount", value_ "50", autocomplete_ "off"]
          select_ [autocomplete_ "off", name_ "share-type"] $ do
            option_ [value_ "percentage", selected_ "selected"] "%"
            option_ [value_ "fixed", autocomplete_ "off"] "kr"
          span_ [class_ "form-comment"] "av"
        div_ [class_ "debtor-form-group"] $ do
          input_ [type_ "number", id_ "total", name_ "total", min_ "0", autocomplete_ "off"]
          span_ "kr"
          span_ "*"
      small_ "*Resten betalas av den andre."
      button_ [type_ "submit", hxPost_ "/split/lagg-till", hxTarget_ "#tally-expenses-container", hxSwap_ "outerHTML"] "Lägg till"
    transactions_ expenses

transactions_ :: (Monad m) => [Transaction] -> HtmlT m ()
transactions_ transactions = div_ [id_ "tally-expenses-container"] $ do
  h2_ "Skulder"
  div_ [class_ "tally-container"] $ do
    mapM_ iou_ $ debtsToList $ simplifiedDebts transactions
  button_ [type_ "submit", hxPost_ "/split/gor-upp", hxTarget_ "#tally-expenses-container", hxSwap_ "outerHTML"] "Gör upp"
  h2_ "Utgifter"
  div_ [class_ "expenses-container"] $ do
    mapM_ transaction_ transactions

data Transactions = Transactions [Transaction]

instance ToHtml Transactions where
  toHtml (Transactions transactions) = transactions_ transactions
  toHtmlRaw = toHtml

iou_ :: (Monad m) => (Person, [(Person, Amount)]) -> HtmlT m ()
iou_ (p, ious') = div_ [class_ "debt-container"] $ do
  h3_ [class_ "debt-title", style_ $ "background-color: " <> p.color] $ toHtml p.name <> " är skyldig:"
  ul_ $
    mapM_ debtItem_ ious'

debtItem_ :: (Monad m) => (Person, Amount) -> HtmlT m ()
debtItem_ (p, amount) = li_ $ toHtml $ p.name <> ": " <> T.pack (showFFloat (Just 2) amount "kr")

data SplitPage = SplitPage [Transaction]

instance ToHtml SplitPage where
  toHtml (SplitPage exps) = splitPage_ exps
  toHtmlRaw = toHtml

data EditExpensePage = EditExpensePage {date :: Text}

instance ToHtml EditExpensePage where
  toHtml (EditExpensePage date) = editExpensePage_ date
  toHtmlRaw = toHtml

editExpensePage_ :: (Monad m) => Text -> HtmlT m ()
editExpensePage_ date = baseTemplate $ do
  h1_ "Redigera utgift"
  span_ $ toHtml $ "Datum: " <> date

encodeDate :: (Show a) => a -> Text
encodeDate = T.pack . filter isNumber . show

transaction_ :: (Monad m) => Transaction -> HtmlT m ()
transaction_ (ExpenseTransaction exp@(Expense {paidBy, total, rubric, date})) = div_ [class_ "expense-container"] $ do
  div_ [class_ "expense-info-container"] $ do
    h3_ [class_ "expense-title", title_ rubric] $ do
      toHtml rubric
    div_ [class_ "date-container"] $ do
      span_ $ toHtml $ formatDate date
      span_ [class_ "paid-by", style_ $ "background-color: " <> paidBy.color] $ toHtml $ show paidBy
      a_ [class_ "edit-link", href_ $ "/split/redigera/" <> encodeDate date] "Redigera"
  div_ [class_ "expense-info-container no-shrink"] $ do
    span_ $ toHtml $ show total <> "kr"
    split_ exp
transaction_ (SettlementTransaction (Settlement {from, to, amount, date})) = div_ [class_ "expense-container"] $ do
  div_ [class_ "expense-info-container"] $ do
    h3_ [class_ "expense-title", title_ $ "Swish till " <> to.name] $ "Swish till " <> toHtml to
    div_ [class_ "date-container"] $ do
      span_ $ toHtml $ formatDate date
      span_ [class_ "paid-by", style_ $ "background-color: " <> from.color] $ toHtml $ show from
  div_ [class_ "expense-info-container"] $ do
    span_ $ toHtml $ show amount <> "kr"
    -- right arrow
    i_ [class_ "settle-icons"] "💸"

split_ :: (Monad m) => Expense -> HtmlT m ()
split_ expense = div_ [class_ "split-container"] $ do
  pieChart_ expense 50

pieChart_ :: (Monad m) => Expense -> Int -> HtmlT m ()
pieChart_ exp size =
  div_
    [ style_ $
        "border-radius: 50%; width:"
          <> size'
          <> "px; height: "
          <> size'
          <> "px; background-image: conic-gradient("
          <> colorShares exp.total exp.split.shares
          <> ");",
      class_ "pie-chart"
    ]
    ""
  where
    size' = T.pack (show size)
    colorShare :: Text -> Float -> Text
    colorShare col sh = col <> " " <> text sh <> "%"
    colorShares :: Amount -> [Share] -> Text
    colorShares total =
      T.intercalate ", "
        . snd
        . foldl' (genColorText total) (0, [])

    genColorText :: Amount -> (Float, [Text]) -> Share -> (Float, [Text])
    genColorText _ (sum', txt) (Percentage p sh) = (sum' + sh, txt <> [colorShare p.color sum', colorShare p.color (sum' + sh)])
    genColorText total (sum', txt) (Fixed p sh) = (sum' + toPercent sh, txt <> [colorShare p.color sum', colorShare p.color (sum' + toPercent sh)])
      where
        toPercent :: Amount -> Float
        toPercent = (* 100) . (/ total)

text :: (Show a) => a -> Text
text = T.pack . show
