{- | This module exposes data types with ToHTML instances for the server's
endpoints, but also an onclick event for a product.
-}
module Server.Html (
    Checkbox (..),
    EditExpensePage (..),
    Feedback (..),
    FeedbackMessage (..),
    Page404 (..),
    ProductSearchList (..),
    Search (..),
    ShoppingItem (..),
    ShoppingItems (..),
    ShoppingPage (..),
    SplitPage (..),
    Transactions (..),
    addToShoppingList,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON,
    encode,
 )
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (toStrict)
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Deriving.Aeson (CustomJSON (..))
import GHC.Generics (Generic)
import Inter.Language (Language, flag, mkApiHref, mkHref, toHref)
import Inter.Lexicon (l, l_)
import Inter.Lexicon qualified as Lexicon
import Lucid
import Lucid.Base qualified
import Lucid.Htmx qualified as HX
import Numeric (showFFloat)
import Safe (headMay)
import Split
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Willys.Response

newtype Search = Search {unSearch :: Text}
    deriving stock (Generic, Show)
    deriving newtype (Eq)

instance FromForm Search where
    fromForm form = Search <$> parseUnique "query" form

newtype Products = Products {products :: [Product]}
    deriving stock (Generic, Show, Eq)

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

baseTemplate :: (Monad m) => Language -> HtmlT m b -> HtmlT m b
baseTemplate lang content = baseTemplate' (navbar_ lang >> content)

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

data Page404 = Page404 !Language !Text
    deriving stock (Show, Eq)

instance ToHtml Page404 where
    toHtml (Page404 lang mtext) = baseTemplate lang $ do
        h1_ "404"
        p_ $ toHtml $ mtext
    toHtmlRaw = toHtml

navbar_ :: (Monad m) => Language -> HtmlT m ()
navbar_ lang = nav_ $ ul_ $ do
    span_ [id_ "language-links"] $ do
        mapM_
            (\(lang', flag') -> li_ (a_ [class_ "language-link", href_ (toHref lang')] flag'))
            (zip (enumFrom minBound) (map (toHtml . flag) (enumFrom minBound)))
    li_
        ( a_
            [href_ $ mkHref lang "/inkop"]
            (l_ lang Lexicon.NavbarShoppingList)
        )
    li_
        ( a_
            [href_ $ mkHref lang "/split"]
            (l_ lang Lexicon.NavbarSplit)
        )

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
                            , src_ (Maybe.fromMaybe "" $ unImageUrl (productImage p))
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

addToShoppingList :: Language -> Product -> [Attribute]
addToShoppingList lang p =
    [ HX.hxPost_ (mkHref lang "/inkop/lagg-till")
    , HX.hxTarget_ "#shopping-list"
    , HX.hxSwap_ "outerHTML"
    , HX.hxExt_ "json-enc"
    , HX.hxVals_ (TE.decodeUtf8 $ toStrict $ encode p)
    ]

data ShoppingPage
    = ShoppingPage !Language ![Product] ![Promotion] !(Maybe [ShoppingItem])
    deriving stock (Show, Eq)

instance ToHtml ShoppingPage where
    toHtml (ShoppingPage lang products promotions shoppingList) = baseTemplate lang $ do
        h1_ $ l_ lang Lexicon.WeeksShoppingList
        div_ [class_ "tabs"] $ do
            button_
                [ id_ "default-open"
                , class_ "tab"
                , onclick_ "openTab(event, 'shopping-list-container')"
                ]
                (l_ lang Lexicon.ShoppingList)
            button_
                [ class_ "tab"
                , onclick_ "openTab(event, 'promotions-container')"
                ]
                (l_ lang Lexicon.Offers)
            button_
                [ class_ "tab"
                , onclick_ "openTab(event, 'product-search-container')"
                ]
                (l_ lang Lexicon.Search)
        div_ [id_ "product-search-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.SearchAndAddProduct)
            form_ [class_ "gapped-form"] $
                productSearch_ lang mempty (mkApiHref "/inkop/produkter") products
        div_ [id_ "promotions-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.WeeksOffers)
            toHtml $
                ProductSearchList
                    (addToShoppingList lang)
                    (coerce promotions)
                    (l lang Lexicon.Offers)
                    "promotion-products"
        div_ [id_ "shopping-list-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.YourShoppingList)
            div_ [class_ "shopping-list-buttons"] $ do
                button_
                    [ class_ "remove-all-button"
                    , type_ "button"
                    , HX.hxDelete_ $ mkApiHref "/inkop/ta-bort-alla"
                    , HX.hxTarget_ "#shopping-list"
                    , HX.hxSwap_ "outerHTML"
                    ]
                    (l_ lang Lexicon.RemoveAll)
                button_
                    [ class_ "remove-checked-button"
                    , type_ "button"
                    , HX.hxDelete_ $ mkApiHref "/inkop/ta-bort"
                    , HX.hxTarget_ "#shopping-list"
                    , HX.hxSwap_ "outerHTML"
                    ]
                    (l_ lang Lexicon.RemoveMarked)
            case shoppingList of
                Nothing -> p_ (l_ lang Lexicon.SomethingWentWrong)
                Just list -> toHtml (ShoppingItems list)
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

newtype ShoppingItems = ShoppingItems {unShoppingItems :: [ShoppingItem]}
    deriving stock (Generic, Show)
    deriving newtype (Eq)

instance ToHtml ShoppingItems where
    toHtmlRaw = toHtml
    toHtml (ShoppingItems items) = div_
        [ id_ "shopping-list"
        , HX.hxExt_ "sse"
        , hxSseConnect_ (mkApiHref "/inkop/sse")
        , hxSseSwap_ "message"
        ]
        $ do
            mapM_ shoppingItem_ items

shoppingItem_ :: (Monad m) => ShoppingItem -> HtmlT m ()
shoppingItem_ item = div_ [class_ "shopping-item", id_ divId] $ do
    img_ [class_ "item-image", src_ (Maybe.fromMaybe "" $ unImageUrl (productImage (siProduct item)))]
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
            , HX.hxPost_ (mkApiHref "/inkop/toggla")
            , HX.hxExt_ "json-enc"
            , HX.hxVals_ (TL.toStrict $ encodeToLazyText (siProduct item))
            , autocomplete_ "off"
            ]
                <> if (siCheck item) == Checked then [checked_] else []
  where
    divId = "shopping-item-" <> getId (siProduct item)

productSearch_ ::
    (Monad m) =>
    Language ->
    -- | onclick for a product
    (Product -> [Attribute]) ->
    -- | POST url
    Text ->
    [Product] ->
    HtmlT m ()
productSearch_ lang attributes posturl products = do
    div_ [class_ "form-group"] $ do
        button_ [type_ "submit", disabled_ "true", style_ "display: none"] ""
        label_ [for_ "query"] $ l_ lang Lexicon.Product
        input_
            [ placeholder_ (l lang Lexicon.SearchForAProduct)
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
            (l_ lang Lexicon.Show)
    toHtml (ProductSearchList attributes products (l lang Lexicon.SearchResults) listId)
  where
    listId = "searched-products"

-- | Attribute for specifying the URL of the SSE server
hxSseConnect_ :: Text -> Attribute
hxSseConnect_ = Lucid.Base.makeAttribute "sse-connect"

-- | Attribute for specifying the name of the message to swap into the DOM
hxSseSwap_ :: Text -> Attribute
hxSseSwap_ = Lucid.Base.makeAttribute "sse-swap"

data SplitPage = SplitPage !Language ![Transaction]

instance ToHtml SplitPage where
    toHtmlRaw = toHtml
    toHtml (SplitPage lang expenses) = baseTemplate lang $ do
        h1_ "Splitvajs"
        fieldset_ $ do
            legend_ (l_ lang Lexicon.AddExpenseLegend)
            form_
                [ class_ "gapped-form"
                , id_ "split-form"
                , autocomplete_ "off"
                ]
                $ do
                    div_ [class_ "form-group"] $ do
                        label_ [for_ "rubric"] (l_ lang Lexicon.ExpenseRubric)
                        input_ [type_ "text", id_ "rubric", name_ "rubric"]
                    fieldset_ [class_ "radio-form-group"] $ do
                        legend_ (l_ lang Lexicon.Paid)
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
                                label_
                                    [for_ (personName p)]
                                    (toHtml (personName p))
                            )
                            people
                    fieldset_ [class_ "debt-form-group"] $ do
                        legend_ (l_ lang Lexicon.Debt)
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
                                            (toHtml (personName p))
                                    )
                                    people
                            span_ [class_ "form-comment"] (l_ lang Lexicon.Pays)
                        div_ [class_ "debtor-form-group"] $ do
                            input_
                                [ type_ "number"
                                , id_ "amount"
                                , name_ "amount"
                                , value_ "50"
                                ]
                            select_ [name_ "share-type"] $ do
                                option_
                                    [ value_ (shareTypeToText Percentage)
                                    , selected_ "selected"
                                    ]
                                    (toHtml (shareTypeSymbol lang Percentage))
                                option_
                                    [value_ (shareTypeToText Fixed)]
                                    (toHtml (shareTypeSymbol lang Fixed))
                            span_ [class_ "form-comment"] (l_ lang Lexicon.Of)
                        div_ [class_ "debtor-form-group"] $ do
                            input_
                                [ type_ "number"
                                , id_ "total"
                                , name_ "total"
                                , min_ "0"
                                ]
                            span_ $ l_ lang Lexicon.Currency <> "*"
                    small_ $ l_ lang Lexicon.RestIsPaidByOther
                    button_
                        [ type_ "submit"
                        , HX.hxPost_ (mkHref lang "/split/lagg-till")
                        , HX.hxTarget_ "#tally-expenses-container"
                        , HX.hxSwap_ "outerHTML"
                        ]
                        (l_ lang Lexicon.Add)
        toHtml (Transactions lang expenses)

data Transactions = Transactions Language [Transaction]

instance ToHtml Transactions where
    toHtmlRaw = toHtml
    toHtml (Transactions lang transactions) = div_
        [id_ "tally-expenses-container"]
        $ do
            h2_ (l_ lang Lexicon.Debts)
            if null settles
                then p_ (l_ lang Lexicon.NoDebts)
                else do
                    div_ [class_ "tally-container"] $ mapM_ (iou_ lang) settles
                    button_
                        [ type_ "submit"
                        , HX.hxPost_ (mkHref lang "/split/gor-upp")
                        , HX.hxTarget_ "#tally-expenses-container"
                        , HX.hxSwap_ "outerHTML"
                        ]
                        (l_ lang Lexicon.Settle)
            h2_ (l_ lang Lexicon.Expenses)
            if null transactions
                then p_ (l_ lang Lexicon.NoExpenses)
                else div_ [class_ "expenses-container"] $ do
                    mapM_ (toHtml . TransactionHtml lang) transactions
      where
        settles = simplifiedDebts transactions

iou_ :: (Monad m) => Language -> (Person, [(Person, Amount)]) -> HtmlT m ()
iou_ lang (p, ious') = do
    span_
        [ class_ "creditor-name"
        , style_ $ "background-color: " <> personColor p
        ]
        $ toHtml
        $ (personName p) <> " " <> l lang Lexicon.IsOwed
    mapM_ (debtItem_ lang) ious'

debtItem_ :: (Monad m) => Language -> (Person, Amount) -> HtmlT m ()
debtItem_ lang (p, amount) =
    toHtml $
        personName p
            <> ": "
            <> Text.pack
                ( showFFloat
                    (Just 2)
                    (unAmount amount)
                    (" " <> Text.unpack (l lang Lexicon.Currency))
                )

data EditExpensePage = EditExpensePage !Language !Expense !Share !(Maybe FeedbackMessage)
    deriving stock (Show, Eq)

instance ToHtml EditExpensePage where
    toHtmlRaw = toHtml
    toHtml (EditExpensePage lang e debtorShare message) = baseTemplate lang $ do
        h1_ (l_ lang Lexicon.EditExpense)
        form_
            [ class_ "gapped-form"
            , id_ "split-form"
            , autocomplete_ "off"
            ]
            $ do
                div_ [class_ "form-group"] $ do
                    label_ [for_ "rubric"] (l_ lang Lexicon.ExpenseRubric)
                    input_
                        [ type_ "text"
                        , id_ "rubric"
                        , name_ "rubric"
                        , value_ (expenseRubric e)
                        ]
                fieldset_ [class_ "radio-form-group"] $ do
                    legend_ (l_ lang Lexicon.Paid)
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
                            label_ [for_ (personName p)] (toHtml (personName p))
                        )
                        (peopleOfExpense e)
                fieldset_ [class_ "debt-form-group"] $ do
                    legend_ (l_ lang Lexicon.Debt)
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
                                        (toHtml (personName p))
                                )
                                (peopleOfExpense e)
                        span_ [class_ "form-comment"] (l_ lang Lexicon.Pays)
                    div_ [class_ "debtor-form-group"] $ do
                        input_
                            [ type_ "number"
                            , id_ "amount"
                            , name_ "amount"
                            , value_ (text $ unAmount $ shareAmount debtorShare)
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
                                            (toHtml (shareTypeSymbol lang st))
                                    )
                                    [Percentage, Fixed]
                        span_ [class_ "form-comment"] (l_ lang Lexicon.Of)
                    div_ [class_ "debtor-form-group"] $ do
                        input_
                            [ type_ "number"
                            , id_ "total"
                            , name_ "total"
                            , min_ "0"
                            , value_ (text $ unAmount $ expenseTotal e)
                            ]
                        span_ $ l_ lang Lexicon.Currency
                        span_ "*"
                small_ (l_ lang Lexicon.RestIsPaidByOther)
                div_ [id_ "edit-action-buttons"] $ do
                    button_
                        [ type_ "submit"
                        , HX.hxPatch_ (mkHref lang $ "/split/spara/" <> text (expenseId e))
                        , HX.hxTarget_ "body"
                        ]
                        (l_ lang Lexicon.Save)
                    button_
                        [ type_ "button"
                        , HX.hxDelete_ (mkHref lang $ "/split/ta-bort/" <> text (expenseId e))
                        , HX.hxSwap_ "none"
                        ]
                        (l_ lang Lexicon.Remove)
                maybe mempty toHtml message

-- | Convert a share type to text. Useful for use in name attributes in forms.
shareTypeToText :: ShareType -> Text
shareTypeToText Percentage = "percentage"
shareTypeToText Fixed = "fixed"

-- | Symbol for share type. Useful for displaying the share type in the UI.
shareTypeSymbol :: Language -> ShareType -> Text
shareTypeSymbol _ Percentage = "%"
shareTypeSymbol lang Fixed = l lang Lexicon.Currency

data TransactionHtml = TransactionHtml !Language !Transaction

instance ToHtml TransactionHtml where
    toHtmlRaw = toHtml
    toHtml (TransactionHtml lang (ExpenseTransaction e)) =
        div_ [class_ "expense-container"] $ do
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
                        , href_ $ mkHref lang "/split/redigera/" <> text (expenseId e)
                        ]
                        (l_ lang Lexicon.Edit)
            div_ [class_ "expense-data-container no-shrink"] $ do
                span_ $
                    toHtml $
                        text (unAmount (expenseTotal e))
                            <> " "
                            <> l lang Lexicon.Currency
                split_ e
    toHtml (TransactionHtml lang (SettlementTransaction s)) =
        div_ [class_ "expense-container"] $ do
            div_ [class_ "expense-info-container"] $ do
                h3_
                    [ class_ "expense-title"
                    , title_ $
                        l lang Lexicon.PaymentTo
                            <> " "
                            <> personName (settlementTo s)
                    ]
                    $ toHtml
                    $ l lang Lexicon.PaymentTo
                        <> " "
                        <> personName (settlementTo s)
                div_ [class_ "date-container"] $ do
                    span_ $ toHtml $ formatDate $ settlementDate s
                    span_
                        [ class_ "paid-by"
                        , style_ $
                            "background-color: "
                                <> personColor (settlementFrom s)
                        ]
                        $ toHtml
                        $ personName (settlementFrom s)
            div_ [class_ "expense-data-container"] $ do
                span_ $
                    toHtml $
                        text (unAmount (settlementAmount s))
                            <> " "
                            <> l lang Lexicon.Currency
                i_ [class_ "settle-icons"] "💸"

split_ :: (Monad m) => Expense -> HtmlT m ()
split_ expense = div_ [class_ "split-container"] $ do
    pieChart_ expense 50

{- | Pie chart for expense split. Size is in pixels.
Colors are based on the person's color.
-}
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
        ( sum' + unAmount sh
        , txt
            <> [ colorShare (personColor p) sum'
               , colorShare (personColor p) (sum' + unAmount sh)
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
        toPercent = (* 100) . coerce . (/ total)

-- | helper function to convert a Showable to a Text
text :: (Show a) => a -> Text
text = Text.pack . show
