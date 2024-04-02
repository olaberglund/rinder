{-# OPTIONS_GHC -Wno-orphans #-}

module Server (newEnv, app) where

import Control.Concurrent (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
    FromJSON (..),
    ToJSON,
    eitherDecode,
    eitherDecodeStrict,
    encode,
 )
import Data.Aeson.Text (encodeToLazyText)
import Data.Binary.Builder qualified as Builder
import Data.ByteString (toStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time qualified as Time
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Deriving.Aeson (CustomJSON (..))
import GHC.Generics (Generic)
import Lucid
import Lucid.Base qualified
import Lucid.Htmx qualified as HX
import Network.HTTP.Types (hLocation)
import Network.Wai.EventSource (ServerEvent (..))
import Numeric (showFFloat)
import Safe (headMay)
import Servant (
    Application,
    Capture,
    Context (EmptyContext, (:.)),
    Delete,
    ErrorFormatters (notFoundErrorFormatter),
    FormUrlEncoded,
    GenericMode (type (:-)),
    Get,
    Handler,
    JSON,
    NamedRoutes,
    NoContent (..),
    NoFraming,
    Patch,
    Post,
    Proxy (Proxy),
    Raw,
    ReqBody,
    ServerError (errBody, errHeaders),
    StreamGet,
    defaultErrorFormatters,
    err303,
    err404,
    err500,
    serveDirectoryWebApp,
    serveWithContext,
    throwError,
    type (:>),
 )
import Servant.API.EventStream (EventSource, EventStream)
import Servant.HTML.Lucid (HTML)
import Servant.Server.Generic (AsServer)
import Servant.Types.SourceT qualified as S
import Splitvajs (
    Amount,
    Expense (
        expenseDate,
        expenseId,
        expensePaidBy,
        expenseRubric,
        expenseSplit,
        expenseTotal
    ),
    ExpenseForm,
    Person (personColor, personName),
    Settlement (
        settlementAmount,
        settlementDate,
        settlementFrom,
        settlementTo
    ),
    Share (Share, shareAmount, sharePerson, shareType),
    ShareType (Fixed, Percentage),
    Split (splitShares),
    Transaction (..),
    findExpense,
    formatDate,
    people,
    peopleOfExpense,
    settlements,
    shareTypeSymbol,
    shareTypeToText,
    simplifiedDebts,
    singleDebtor,
    toExpense,
 )
import System.Timeout qualified
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Willys (
    Product (..),
    Promotion (..),
    StripAndLower,
    fetchProducts,
    fetchPromotions,
    getId,
    getPrice,
    getSavePrice,
    runClientDefault,
    unImageUrl,
 )

newtype Search = Search {unSearch :: Text}
    deriving stock (Generic, Show)
    deriving newtype (Eq)

instance FromForm Search where
    fromForm form = Search <$> parseUnique "query" form

newtype Products = Products {products :: [Product]}
    deriving stock (Generic, Show, Eq)

data Env = Env
    { broadcastChan :: !(Chan ServerEvent)
    }

newEnv :: IO Env
newEnv = Env <$> newChan

{- FOURMOLU_DISABLE -}
data RootApi as = RootAPI
    { homePageEP :: !(as :- Get '[HTML] NoContent)
    , staticEP :: !(as :- "static" :> Raw)
    , shoppingEP :: !(as :- "inkop" :> NamedRoutes ShoppingApi)
    , splitEP :: !(as :- "split" :> NamedRoutes SplitApi)
    }
    deriving stock (Generic)

data ShoppingApi as = ShoppingApi
    { shoppingPageEP :: !(as :- Get '[HTML] ShoppingPage)
    , removeCheckedEP :: !(as :- "ta-bort" :> Delete '[HTML] [ShoppingItem])
    , removeAllEP :: !(as :- "ta-bort-alla" :> Delete '[HTML] [ShoppingItem])
    , sseEP :: !(as :- "sse" :> StreamGet NoFraming EventStream EventSource)
    , productListEP 
        :: !(as 
        :- "produkter" 
        :> ReqBody '[FormUrlEncoded] Search 
        :> Post '[HTML] ProductSearchList)
    , addProductEP 
        :: !(as :- "lagg-till" 
        :> ReqBody '[JSON] Product 
        :> Post '[HTML] [ShoppingItem])
    , toggleProductEP 
        :: !(as :- "toggla" 
        :> ReqBody '[JSON] Product 
        :> Post '[HTML] NoContent)
    }
    deriving stock (Generic)

data SplitApi as = SplitApi
    { splitPageEP :: !(as :- Get '[HTML] SplitPage)
    , settleUpEP :: !(as :- "gor-upp" :> Post '[HTML] Transactions)
    , removeExpenseEp 
        :: !(as 
        :- "ta-bort" 
        :> Capture "id" UUID 
        :> Delete '[HTML] NoContent)
    , newExpenseEP 
        :: !(as :- "lagg-till" 
        :> ReqBody '[FormUrlEncoded] ExpenseForm 
        :> Post '[HTML] Transactions)
    , editExpensePageEP 
        :: !(as 
        :- "redigera" 
        :> Capture "id" UUID 
        :> Get '[HTML] EditExpensePage)
    , saveExpenseEP 
        :: !(as 
        :- "spara" 
        :> Capture "id" UUID 
        :> ReqBody '[FormUrlEncoded] ExpenseForm 
        :> Patch '[HTML] EditExpensePage)
    }
    deriving stock (Generic)

{- FOURMOLU_ENABLE -}

data Feedback = Success | Failure
    deriving stock (Show, Eq)

data FeedbackMessage = FeedbackMessage !Feedback !Text
    deriving stock (Show, Eq)

instance ToHtml FeedbackMessage where
    toHtml (FeedbackMessage Success msg) =
        span_ [classes_ ["toast", "toast-success"]] $
            toHtml msg
    toHtml (FeedbackMessage Failure msg) =
        span_ [classes_ ["toast", "toast-fail"]] $
            toHtml msg
    toHtmlRaw = toHtml

app :: Env -> Application
app env =
    serveWithContext
        (Proxy @(NamedRoutes RootApi))
        (customFormatters :. EmptyContext)
        (server env)
  where
    customFormatters :: ErrorFormatters
    customFormatters =
        defaultErrorFormatters
            { notFoundErrorFormatter = const (err404' Nothing)
            }

err404' :: Maybe Text -> ServerError
err404' msg =
    err404
        { errBody =
            renderBS $
                toHtml (Page404 (Maybe.fromMaybe "Inget att se här..." msg))
        }

server :: Env -> RootApi AsServer
server env =
    RootAPI
        { homePageEP = redirect "/split"
        , staticEP = serveDirectoryWebApp "static"
        , shoppingEP =
            ShoppingApi
                { shoppingPageEP = shoppingPageH
                , productListEP = productListH addToShoppingList
                , addProductEP = addProductH env
                , toggleProductEP = toggleProductH env
                , removeCheckedEP = removeCheckedH env
                , removeAllEP = removeAllH
                , sseEP = sseH env
                }
        , splitEP =
            SplitApi
                { splitPageEP = splitPageH
                , newExpenseEP = newExpenseH
                , settleUpEP = settleUpH
                , editExpensePageEP = editExpensePageH
                , saveExpenseEP = saveExpenseH
                , removeExpenseEp = removeExpenseH
                }
        }

transactionsFile :: FilePath
transactionsFile = "transactions.json"

shoppingListFile :: FilePath
shoppingListFile = "shopping-list.json"

removeExpenseH :: UUID -> Handler NoContent
removeExpenseH uuid = do
    res <- liftIO $ BS.readFile transactionsFile
    case eitherDecodeStrict res of
        Right ts -> do
            liftIO $ LBS.writeFile transactionsFile (encode (deleteExpense ts))
            hxRedirect "/split"
        Left err -> liftIO (print err) >> throwError err500
  where
    deleteExpense :: [Transaction] -> [Transaction]
    deleteExpense [] = []
    deleteExpense (x : xs)
        | ExpenseTransaction e <- x, uuid == expenseId e = xs
        | otherwise = x : deleteExpense xs

saveExpenseH :: UUID -> ExpenseForm -> Handler EditExpensePage
saveExpenseH uuid form = do
    res <- liftIO $ BS.readFile transactionsFile
    case eitherDecodeStrict res of
        Right ts -> do
            let newTs = map replaceExpense ts
            liftIO $ LBS.writeFile transactionsFile (encode newTs)
            let mexp = findExpense uuid newTs
            case (mexp, mexp >>= singleDebtor) of
                (Just e, Just debtor) ->
                    return $
                        EditExpensePage
                            e
                            debtor
                            (Just (FeedbackMessage Success "Utgift sparad"))
                _ -> throwError $ err404' (Just "Ingen sådan utgift hittades")
        Left err -> liftIO (print err) >> throwError err500
  where
    replaceExpense :: Transaction -> Transaction
    replaceExpense (ExpenseTransaction e) =
        if expenseId e == uuid
            then
                ExpenseTransaction
                    (toExpense form (expenseId e) (expenseDate e))
            else ExpenseTransaction e
    replaceExpense t = t

editExpensePageH :: UUID -> Handler EditExpensePage
editExpensePageH uuid = do
    res <- liftIO $ BS.readFile transactionsFile
    case eitherDecodeStrict res of
        Right ts -> do
            let expense = findExpense uuid ts
            case expense of
                Just e -> case singleDebtor e of
                    Just share -> return $ EditExpensePage e share Nothing
                    Nothing -> throwError $ err404' (Just moreThanTwoError)
                Nothing ->
                    throwError $
                        err404' (Just "Ingen sådan utgift hittades")
        Left err -> liftIO (print err) >> throwError err500
  where
    moreThanTwoError =
        "Just nu stöds inte \
        \redigering av utgifter med fler deltagare än två"

settleUpH :: Handler Transactions
settleUpH = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    writeTransactionsHandlerHelper
        (\ts -> map SettlementTransaction (settlements tz utc ts) <> ts)

newExpenseH :: ExpenseForm -> Handler Transactions
newExpenseH form = do
    utc <- liftIO Time.getCurrentTime
    tz <- liftIO Time.getCurrentTimeZone
    uuid <- liftIO UUID.nextRandom
    writeTransactionsHandlerHelper
        ( ExpenseTransaction
            (toExpense form uuid (Time.utcToLocalTime tz utc))
            :
        )

writeTransactionsHandlerHelper ::
    ([Transaction] -> [Transaction]) ->
    Handler Transactions
writeTransactionsHandlerHelper genNewTs = liftIO $ do
    res <- BS.readFile transactionsFile
    case eitherDecodeStrict res of
        Right ts -> do
            let newTs = genNewTs ts
            LBS.writeFile transactionsFile (encode newTs)
            return (Transactions newTs)
        Left err -> print err >> return (Transactions [])

splitPageH :: Handler SplitPage
splitPageH = liftIO $ do
    res <- BS.readFile transactionsFile
    case eitherDecodeStrict res of
        Right ts -> return (SplitPage ts)
        Left err -> print err >> return (SplitPage [])

sseH :: Env -> Handler EventSource
sseH env = liftIO $ do
    chan <- dupChan (broadcastChan env)
    return $ S.fromStepT (S.Yield keepAlive (rest chan))
  where
    rest :: Chan ServerEvent -> S.StepT IO ServerEvent
    rest chan = S.Effect $ do
        msg <- System.Timeout.timeout (15 * 1000000) (readChan chan)
        return $ case msg of
            Just m -> S.Yield m (rest chan)
            Nothing -> S.Yield keepAlive (rest chan)

    keepAlive :: ServerEvent
    keepAlive = CommentEvent (Builder.fromByteString "keep-alive")

toggle :: Checkbox -> Checkbox
toggle Checked = Unchecked
toggle Unchecked = Checked

removeAllH :: Handler [ShoppingItem]
removeAllH = liftIO $ LBS.writeFile shoppingListFile "[]" >> return []

removeCheckedH :: Env -> Handler [ShoppingItem]
removeCheckedH env = liftIO $ do
    res <- BS.readFile shoppingListFile
    case eitherDecodeStrict res of
        Right ps ->
            let newItems = filter ((== Unchecked) . siCheck) ps
             in updateAndBroadCast env newItems
        Left err -> print err >> return []

toggleProductH :: Env -> Product -> Handler NoContent
toggleProductH env product' = liftIO $ do
    res <- BS.readFile shoppingListFile
    case eitherDecodeStrict res of
        Right ps -> void $ updateAndBroadCast env (map toggleItem ps)
        Left err -> print err
    return NoContent
  where
    toggleItem :: ShoppingItem -> ShoppingItem
    toggleItem i
        | siProduct i == product' = i{siCheck = toggle (siCheck i)}
        | otherwise = i

asServerEvent :: (ToHtml a) => [a] -> ServerEvent
asServerEvent =
    ServerEvent Nothing Nothing
        . map (Builder.fromLazyByteString . renderBS . toHtml)

shoppingPageH :: Handler ShoppingPage
shoppingPageH = liftIO $ do
    fetchedPromotions <- runClientDefault fetchPromotions
    shoppingItems <- eitherDecode <$> LBS.readFile shoppingListFile
    case shoppingItems of
        Left err -> putStrLn err >> return (ShoppingPage mempty mempty mempty)
        Right list ->
            return
                ( ShoppingPage
                    mempty
                    (Either.fromRight mempty fetchedPromotions)
                    (Just list)
                )

addProductH :: Env -> Product -> Handler [ShoppingItem]
addProductH env product' = liftIO $ do
    res <- BS.readFile shoppingListFile
    case eitherDecodeStrict res of
        Right ps ->
            let newList = ShoppingItem product' Unchecked : ps
             in updateAndBroadCast env newList
        Left err -> print err >> return []

hxRedirect :: BS.ByteString -> Handler a
hxRedirect url = throwError err303{errHeaders = [("HX-Redirect", url)]}

redirect :: BS.ByteString -> Handler a
redirect url = throwError err303{errHeaders = [(hLocation, url)]}

updateAndBroadCast :: Env -> [ShoppingItem] -> IO [ShoppingItem]
updateAndBroadCast env items =
    LBS.writeFile shoppingListFile (encode items)
        >> writeChan (broadcastChan env) (asServerEvent items)
        >> return items

productListH ::
    (Product -> [Attribute]) ->
    Search ->
    Handler ProductSearchList
productListH attributes search = liftIO $ do
    res <- runClientDefault (fetchProducts (unSearch search))
    case res of
        Left err ->
            print err
                >> return
                    ( ProductSearchList
                        mempty
                        mempty
                        "Sökresultat"
                        "searched-products"
                    )
        Right products ->
            return $
                ProductSearchList
                    attributes
                    products
                    "Sökresultat"
                    "searched-products"

baseTemplate :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate content = baseTemplate' (navbar_ >> content)

baseTemplate' :: (Monad m) => HtmlT m b -> HtmlT m b
baseTemplate' content = do
    doctype_
    html_ $ do
        head_ $ do
            HX.useHtmx
            HX.useHtmxExtension "json-enc"
            HX.useHtmxExtension "sse"
            link_ [rel_ "stylesheet", href_ ("/static/styles.css")]
            link_
                [ rel_ "icon"
                , type_ "image/png"
                , href_ "/static/images/favicon.ico"
                ]
            meta_ [charset_ "utf-8"]
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1"
                ]
            script_
                [ defer_ "true"
                , type_ "text/javascript"
                , src_ "/static/scripts.js"
                ]
                ("" :: Text)
            title_ "Hörlund"
        body_ $ do
            content
  where

data Page404 = Page404 !Text
    deriving stock (Show, Eq)

instance ToHtml Page404 where
    toHtml (Page404 mtext) = baseTemplate $ do
        h1_ "404"
        p_ $ toHtml $ mtext
    toHtmlRaw = toHtml

navbar_ :: (Monad m) => HtmlT m ()
navbar_ = nav_ $ ul_ $ do
    li_ (a_ [href_ "/inkop"] "Inköpslista")
    li_ (a_ [href_ "/split"] "Split")

data ProductSearchList
    = ProductSearchList
        !(Product -> [Attribute])
        ![Product]
        !Text
        !Text
    deriving stock (Generic)

instance ToHtml ProductSearchList where
    toHtmlRaw = toHtml
    toHtml (ProductSearchList attributes products rubric listId) =
        fieldset_ [class_ "products", id_ listId] $ do
            legend_ (toHtml rubric)
            mapM_
                ( \p -> div_
                    ( [ class_ "product-container"
                      , title_ (productName p)
                      ]
                        <> (attributes p)
                    )
                    $ do
                        img_
                            [ class_ "product"
                            , src_ (unImageUrl (productImage p))
                            ]
                        div_ [class_ "product-details"] $ do
                            span_ [class_ "product-name"] $
                                toHtml (productName p)
                            span_ [class_ "product-promo"] $
                                toHtml $
                                    getPrice p
                            span_ [class_ "product-save"] $
                                toHtml $
                                    Maybe.fromMaybe "" $
                                        getSavePrice p
                )
                products

addToShoppingList :: Product -> [Attribute]
addToShoppingList p =
    [ HX.hxPost_ "/inkop/lagg-till"
    , HX.hxTarget_ "#shopping-list"
    , HX.hxSwap_ "outerHTML"
    , HX.hxExt_ "json-enc"
    , HX.hxVals_ (TE.decodeUtf8 $ toStrict $ encode p)
    ]

data ShoppingPage
    = ShoppingPage ![Product] ![Promotion] !(Maybe [ShoppingItem])
    deriving stock (Show, Eq)

instance ToHtml ShoppingPage where
    toHtml (ShoppingPage products promotions shoppingList) = baseTemplate $ do
        h1_ "Veckans inköpslista"
        div_ [class_ "tabs"] $ do
            button_
                [ id_ "default-open"
                , class_ "tab"
                , onclick_ "openTab(event, 'shopping-list-container')"
                ]
                "Inköpslista"
            button_
                [ class_ "tab"
                , onclick_ "openTab(event, 'promotions-container')"
                ]
                "Erbjudanden"
            button_
                [ class_ "tab"
                , onclick_ "openTab(event, 'product-search-container')"
                ]
                "Sök"
        div_ [id_ "product-search-container", class_ "tabcontent"] $ do
            h2_ "Sök och lägg till produkter"
            form_ [class_ "gapped-form"] $
                productSearch_ mempty "/inkop/produkter" products
        div_ [id_ "promotions-container", class_ "tabcontent"] $ do
            h2_ "Veckans erbjudanden"
            toHtml $
                ProductSearchList
                    addToShoppingList
                    (coerce promotions)
                    "Erbjudanden"
                    "promotion-products"
        div_ [id_ "shopping-list-container", class_ "tabcontent"] $ do
            h2_ "Din inköpslista"
            div_ [class_ "shopping-list-buttons"] $ do
                button_
                    [ class_ "remove-all-button"
                    , type_ "button"
                    , HX.hxDelete_ "/inkop/ta-bort-alla"
                    , HX.hxTarget_ "#shopping-list"
                    , HX.hxSwap_ "outerHTML"
                    ]
                    "Ta bort alla"
                button_
                    [ class_ "remove-checked-button"
                    , type_ "button"
                    , HX.hxDelete_ "/inkop/ta-bort"
                    , HX.hxTarget_ "#shopping-list"
                    , HX.hxSwap_ "outerHTML"
                    ]
                    "Ta bort markerade"
            case shoppingList of
                Nothing -> p_ "Något gick fel..."
                Just list -> toHtml list
    toHtmlRaw = toHtml

data Checkbox = Checked | Unchecked
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

data ShoppingItem = ShoppingItem {siProduct :: !Product, siCheck :: !Checkbox}
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON) via StripAndLower "si" ShoppingItem

instance ToHtml ShoppingItem where
    toHtml item = shoppingItem_ item
    toHtmlRaw = toHtml

instance ToHtml [ShoppingItem] where
    toHtmlRaw = toHtml
    toHtml items = div_
        [ id_ "shopping-list"
        , HX.hxExt_ "sse"
        , hxSseConnect_ "/inkop/sse"
        , hxSseSwap_ "message"
        ]
        $ do
            mapM_ shoppingItem_ items

shoppingItem_ :: (Monad m) => ShoppingItem -> HtmlT m ()
shoppingItem_ item = div_ [class_ "shopping-item", id_ divId] $ do
    img_ [class_ "item-image", src_ (unImageUrl (productImage (siProduct item)))]
    div_ [class_ "item-details"] $ do
        div_ [class_ "item-details-text"] $ do
            span_ [class_ "product-name"] $ toHtml (productName (siProduct item))
            span_ [class_ "item-price"] $ toHtml $ getPrice (siProduct item)
            span_ [class_ "item-save"] $
                toHtml $
                    Maybe.fromMaybe "" $
                        getSavePrice (siProduct item)
        input_ $
            [ class_ "item-checkbox"
            , type_ "checkbox"
            , id_ (getId (siProduct item))
            , name_ "name"
            , value_ (productName (siProduct item))
            , HX.hxPost_ "/inkop/toggla"
            , HX.hxExt_ "json-enc"
            , HX.hxVals_ (TL.toStrict $ encodeToLazyText (siProduct item))
            , autocomplete_ "off"
            ]
                <> if (siCheck item) == Checked then [checked_] else []
  where
    divId = "shopping-item-" <> getId (siProduct item)

productSearch_ ::
    (Monad m) =>
    (Product -> [Attribute]) ->
    Text ->
    [Product] ->
    HtmlT m ()
productSearch_ attributes posturl products = do
    div_ [class_ "form-group"] $ do
        button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
        label_ [for_ "query"] "Produkt:"
        input_
            [ placeholder_ "Sök efter en produkt..."
            , id_ "query"
            , list_ "products"
            , name_ "query"
            , type_ "text"
            , autocomplete_ "off"
            ]
        button_
            [ id_ "search-button"
            , type_ "button"
            , HX.hxPost_ posturl
            , HX.hxTarget_ ("#" <> listId)
            , HX.hxSwap_ "outerHTML"
            , HX.hxParams_ "query"
            ]
            "Visa"
    toHtml (ProductSearchList attributes products "Sökresultat" listId)
  where
    listId = "searched-products"

hxSseConnect_ :: Text -> Attribute
hxSseConnect_ = Lucid.Base.makeAttribute "sse-connect"

hxSseSwap_ :: Text -> Attribute
hxSseSwap_ = Lucid.Base.makeAttribute "sse-swap"

data SplitPage = SplitPage ![Transaction]

instance ToHtml SplitPage where
    toHtmlRaw = toHtml
    toHtml (SplitPage expenses) = baseTemplate $ do
        h1_ "Splitvajs"
        fieldset_ $ do
            legend_ "Lägg till en utgift"
            form_
                [ class_ "gapped-form"
                , id_ "split-form"
                , autocomplete_ "off"
                ]
                $ do
                    div_ [class_ "form-group"] $ do
                        label_ [for_ "rubric"] "Rubrik:"
                        input_ [type_ "text", id_ "rubric", name_ "rubric"]
                    fieldset_ [class_ "radio-form-group"] $ do
                        legend_ "Betalare"
                        mapM_
                            ( \p -> div_ [class_ "radio-group"] $ do
                                input_ $
                                    [ type_ "radio"
                                    , id_ (personName p)
                                    , name_ "paidBy"
                                    , value_ (personName p)
                                    ]
                                        <> if Just p == headMay people
                                            then [checked_]
                                            else mempty
                                label_ [for_ (personName p)] (toHtml p)
                            )
                            people
                    fieldset_ [class_ "debt-form-group"] $ do
                        legend_ "Skuld"
                        div_ [class_ "debtor-form-group"] $ do
                            select_ [name_ "debtor"] $ do
                                mapM_
                                    ( \p ->
                                        option_
                                            ( [value_ (personName p)]
                                                <> if personName p == "Ola"
                                                    then [selected_ "selected"]
                                                    else mempty
                                            )
                                            (toHtml p)
                                    )
                                    people
                            span_ [class_ "form-comment"] "ska betala"
                        div_ [class_ "debtor-form-group"] $ do
                            input_
                                [ type_ "number"
                                , id_ "amount"
                                , name_ "amount"
                                , value_ "50"
                                ]
                            select_ [name_ "share-type"] $ do
                                option_
                                    [ value_ "percentage"
                                    , selected_ "selected"
                                    ]
                                    "%"
                                option_ [value_ "fixed"] "kr"
                            span_ [class_ "form-comment"] "av"
                        div_ [class_ "debtor-form-group"] $ do
                            input_
                                [ type_ "number"
                                , id_ "total"
                                , name_ "total"
                                , min_ "0"
                                ]
                            span_ "kr"
                            span_ "*"
                    small_ "*Resten betalas av den andre."
                    button_
                        [ type_ "submit"
                        , HX.hxPost_ "/split/lagg-till"
                        , HX.hxTarget_ "#tally-expenses-container"
                        , HX.hxSwap_ "outerHTML"
                        ]
                        "Lägg till"
        toHtml (Transactions expenses)

instance ToHtml Transactions where
    toHtmlRaw = toHtml
    toHtml (Transactions transactions) = div_
        [id_ "tally-expenses-container"]
        $ do
            h2_ "Skulder"
            if null settles
                then p_ "Inga skulder att visa."
                else do
                    div_ [class_ "tally-container"] $ mapM_ iou_ settles
                    button_
                        [ type_ "submit"
                        , HX.hxPost_ "/split/gor-upp"
                        , HX.hxTarget_ "#tally-expenses-container"
                        , HX.hxSwap_ "outerHTML"
                        ]
                        "Gör upp"
            h2_ "Utgifter"
            if null transactions
                then p_ "Inga utgifter att visa."
                else div_ [class_ "expenses-container"] $ do
                    mapM_ toHtml transactions
      where
        settles = simplifiedDebts transactions

data Transactions = Transactions ![Transaction]
    deriving stock (Show, Eq)

iou_ :: (Monad m) => (Person, [(Person, Amount)]) -> HtmlT m ()
iou_ (p, ious') = do
    span_
        [ class_ "creditor-name"
        , style_ $ "background-color: " <> personColor p
        ]
        $ toHtml (personName p) <> " är skyldig:"
    mapM_ debtItem_ ious'

debtItem_ :: (Monad m) => (Person, Amount) -> HtmlT m ()
debtItem_ (p, amount) =
    toHtml $
        personName p
            <> ": "
            <> Text.pack (showFFloat (Just 2) amount "kr")

data EditExpensePage = EditExpensePage !Expense !Share !(Maybe FeedbackMessage)
    deriving stock (Show, Eq)

instance ToHtml EditExpensePage where
    toHtmlRaw = toHtml
    toHtml (EditExpensePage e debtorShare message) = baseTemplate $ do
        h1_ "Redigera utgift"
        form_
            [ class_ "gapped-form"
            , id_ "split-form"
            , autocomplete_ "off"
            ]
            $ do
                div_ [class_ "form-group"] $ do
                    label_ [for_ "rubric"] "Rubrik:"
                    input_
                        [ type_ "text"
                        , id_ "rubric"
                        , name_ "rubric"
                        , value_ (expenseRubric e)
                        ]
                fieldset_ [class_ "radio-form-group"] $ do
                    legend_ "Betalare"
                    mapM_
                        ( \p -> div_ [class_ "radio-group"] $ do
                            input_ $
                                [ type_ "radio"
                                , id_ (personName p)
                                , name_ "paidBy"
                                , value_ (personName p)
                                ]
                                    <> if p == expensePaidBy e
                                        then [checked_]
                                        else mempty
                            label_ [for_ (personName p)] (toHtml p)
                        )
                        (peopleOfExpense e)
                fieldset_ [class_ "debt-form-group"] $ do
                    legend_ "Skuld"
                    div_ [class_ "debtor-form-group"] $ do
                        select_ [name_ "debtor"] $ do
                            mapM_
                                ( \p ->
                                    option_
                                        ( [value_ (personName p)]
                                            <> if p == sharePerson debtorShare
                                                then [selected_ "selected"]
                                                else []
                                        )
                                        (toHtml p)
                                )
                                (peopleOfExpense e)
                        span_ [class_ "form-comment"] "ska betala"
                    div_ [class_ "debtor-form-group"] $ do
                        input_
                            [ type_ "number"
                            , id_ "amount"
                            , name_ "amount"
                            , value_ (text $ shareAmount debtorShare)
                            ]
                        select_
                            [ name_ "share-type"
                            , value_ (text (shareType debtorShare))
                            ]
                            $ do
                                mapM_
                                    ( \st ->
                                        option_
                                            ( [value_ (shareTypeToText st)]
                                                <> if st == shareType debtorShare
                                                    then [selected_ "selected"]
                                                    else []
                                            )
                                            (toHtml (shareTypeSymbol st))
                                    )
                                    [Percentage, Fixed]
                        span_ [class_ "form-comment"] "av"
                    div_ [class_ "debtor-form-group"] $ do
                        input_
                            [ type_ "number"
                            , id_ "total"
                            , name_ "total"
                            , min_ "0"
                            , value_ (text $ expenseTotal e)
                            ]
                        span_ "kr"
                        span_ "*"
                small_ "*Resten betalas av den andre."
                div_ [id_ "edit-action-buttons"] $ do
                    button_
                        [ type_ "submit"
                        , HX.hxPatch_ ("/split/spara/" <> text (expenseId e))
                        , HX.hxTarget_ "body"
                        ]
                        "Spara"
                    button_
                        [ type_ "button"
                        , HX.hxDelete_ ("/split/ta-bort/" <> text (expenseId e))
                        , HX.hxSwap_ "none"
                        ]
                        "Ta bort"
                maybe mempty toHtml message

instance ToHtml Transaction where
    toHtmlRaw = toHtml
    toHtml (ExpenseTransaction e) = div_ [class_ "expense-container"] $ do
        div_ [class_ "expense-info-container"] $ do
            h3_ [class_ "expense-title", title_ (expenseRubric e)] $ do
                toHtml $ expenseRubric e
            div_ [class_ "date-container"] $ do
                span_ $ toHtml $ formatDate $ expenseDate e
                span_
                    [ class_ "paid-by"
                    , style_ $
                        "background-color: "
                            <> personColor (expensePaidBy e)
                    ]
                    $ toHtml
                    $ personName (expensePaidBy e)
                a_
                    [ class_ "edit-link"
                    , href_ $ "/split/redigera/" <> text (expenseId e)
                    ]
                    "Redigera"
        div_ [class_ "expense-info-container no-shrink"] $ do
            span_ $ toHtml $ show (expenseTotal e) <> "kr"
            split_ e
    toHtml (SettlementTransaction s) = div_ [class_ "expense-container"] $ do
        div_ [class_ "expense-info-container"] $ do
            h3_
                [ class_ "expense-title"
                , title_ $ "Swish till " <> personName (settlementTo s)
                ]
                $ "Swish till " <> toHtml (settlementTo s)
            div_ [class_ "date-container"] $ do
                span_ $ toHtml $ formatDate $ settlementDate s
                span_
                    [ class_ "paid-by"
                    , style_ $ "background-color: " <> personColor (settlementFrom s)
                    ]
                    $ toHtml
                    $ personName (settlementFrom s)
        div_ [class_ "expense-info-container"] $ do
            span_ $ toHtml $ show (settlementAmount s) <> "kr"
            i_ [class_ "settle-icons"] "💸"

split_ :: (Monad m) => Expense -> HtmlT m ()
split_ expense = div_ [class_ "split-container"] $ do
    pieChart_ expense 50

pieChart_ :: (Monad m) => Expense -> Int -> HtmlT m ()
pieChart_ e size =
    div_
        [ style_ $
            "border-radius: 50%; width:"
                <> size'
                <> "px; height: "
                <> size'
                <> "px; background-image: conic-gradient("
                <> colorShares
                    (expenseTotal e)
                    (splitShares (expenseSplit e))
                <> ");"
        , class_ "pie-chart"
        ]
        ""
  where
    size' = Text.pack (show size)
    colorShare :: Text -> Float -> Text
    colorShare col sh = col <> " " <> text sh <> "%"
    colorShares :: Amount -> [Share] -> Text
    colorShares total =
        Text.intercalate ", "
            . snd
            . List.foldl' (genColorText total) (0, [])

    genColorText :: Amount -> (Float, [Text]) -> Share -> (Float, [Text])
    genColorText _ (sum', txt) (Share Percentage p sh _) =
        ( sum' + sh
        , txt
            <> [ colorShare (personColor p) sum'
               , colorShare (personColor p) (sum' + sh)
               ]
        )
    genColorText total (sum', txt) (Share Fixed p sh _) =
        ( sum' + toPercent sh
        , txt
            <> [ colorShare (personColor p) sum'
               , colorShare (personColor p) (sum' + toPercent sh)
               ]
        )
      where
        toPercent :: Amount -> Float
        toPercent = (* 100) . (/ total)

text :: (Show a) => a -> Text
text = Text.pack . show
