module Server.Shopping.Html (
    ShoppingItem (..),
    Checkbox (..),
    ShoppingPage (..),
    ProductSearchList (..),
    Search (unSearch),
    addToShoppingList,
)
where

import Data.Aeson (
    FromJSON (..),
    ToJSON,
    encode,
 )
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (toStrict)
import Data.Coerce (coerce)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Deriving.Aeson (CustomJSON (..))
import GHC.Generics (Generic)
import Inter.Language (Language, mkApiHref, mkHref)
import Inter.Lexicon (l, l_)
import Inter.Lexicon qualified as Lexicon
import Lucid
import Lucid.Base qualified
import Lucid.Htmx (hxSwap_)
import Lucid.Htmx qualified as HX
import Server.Utils.Html (baseTemplate)
import Store.Willys.Response
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)

newtype Search = Search {unSearch :: Text}
    deriving stock (Generic, Show)
    deriving newtype (Eq)

instance FromForm Search where
    fromForm form = Search <$> parseUnique "query" form

newtype Products = Products {products :: [Product]}
    deriving stock (Generic, Show, Eq)

data ProductSearchList
    = ProductSearchList
        !Language
        !(Product -> [Attribute])
        ![Product]
        !Text
        !Text
    deriving stock (Generic)

instance ToHtml ProductSearchList where
    toHtmlRaw = toHtml
    toHtml (ProductSearchList lang attributes products rubric listId) =
        fieldset_ [class_ "products", id_ listId] $ do
            legend_ (toHtml rubric)
            mapM_
                ( \p -> div_
                    [class_ "product-container", title_ (productName p)]
                    $ do
                        div_ [class_ "product-image-container"] $
                            img_
                                [ class_ "product-image"
                                , src_ (Maybe.fromMaybe "" $ unImageUrl (productImage p))
                                ]
                        div_ [class_ "product-details"] $ do
                            div_ [class_ "product-details-text"] $ do
                                span_ [class_ "product-name"] $
                                    toHtml (productName p)
                                span_ [class_ "product-promo"] $
                                    toHtml $
                                        getPrice p
                                span_ [class_ "product-save"] $
                                    toHtml $
                                        Maybe.fromMaybe "" $
                                            getSavePrice p
                            button_
                                (class_ "add-to-shopping-list-button" : attributes p)
                                (l_ lang Lexicon.Add)
                )
                products

addToShoppingList :: Language -> Product -> [Attribute]
addToShoppingList lang p =
    [ HX.hxPost_ (mkHref lang "/inkop/lagg-till")
    , HX.hxTarget_ "#shopping-list"
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
                productSearch_ lang mempty (mkHref lang "/inkop/produkter") products
        div_ [id_ "promotions-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.WeeksOffers)
            toHtml $
                ProductSearchList
                    lang
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
                    , HX.hxDelete_ $ mkHref lang "/inkop/ta-bort-alla"
                    , HX.hxTarget_ "#shopping-list"
                    ]
                    (l_ lang Lexicon.RemoveAll)
                button_
                    [ class_ "remove-checked-button"
                    , type_ "button"
                    , HX.hxDelete_ $ mkApiHref "/inkop/ta-bort"
                    , HX.hxTarget_ "#shopping-list"
                    ]
                    (l_ lang Lexicon.RemoveMarked)
            case shoppingList of
                Nothing -> p_ (l_ lang Lexicon.SomethingWentWrong)
                Just list -> do
                    div_
                        [ id_ "shopping-list"
                        , HX.hxExt_ "sse"
                        , hxSseConnect_ (mkApiHref "/inkop/sse")
                        , hxSseSwap_ "message"
                        ]
                        $ toHtml list
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
    toHtml items = mapM_ shoppingItem_ items

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
    toHtml (ProductSearchList lang attributes products (l lang Lexicon.SearchResults) listId)
  where
    listId = "searched-products"

-- | Attribute for specifying the URL of the SSE server
hxSseConnect_ :: Text -> Attribute
hxSseConnect_ = Lucid.Base.makeAttribute "sse-connect"

-- | Attribute for specifying the name of the message to swap into the DOM
hxSseSwap_ :: Text -> Attribute
hxSseSwap_ = Lucid.Base.makeAttribute "sse-swap"
