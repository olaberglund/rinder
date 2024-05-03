module Server.Shopping.Html (
    ShoppingItem (..),
    ShoppingItems (..),
    Checkbox (..),
    ShoppingPage (..),
    ProductSearchList (..),
    Search (unSearch),
    Note (..),
    addToShoppingList,
    Reordering (..),
    Direction (..),
)
where

import           Data.Aeson            (FromJSON (..), ToJSON, encode)
import           Data.Aeson.Text       (encodeToLazyText)
import           Data.ByteString       (toStrict)
import           Data.Coerce           (coerce)
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.Lazy        as TL
import           Deriving.Aeson        (CustomJSON (..))
import           GHC.Generics          (Generic)
import           Inter.Language        (Language, mkHref)
import           Inter.Lexicon         (l, l_)
import qualified Inter.Lexicon         as Lexicon
import           Lucid
import qualified Lucid.Base
import qualified Lucid.Htmx            as HX
import           Server.Utils.Html     (baseTemplate)
import           Store.Grocery
import           Store.Willys.Response (StripAndLower)
import           Web.FormUrlEncoded    (FromForm, fromForm, parseUnique)

newtype Search = Search {unSearch :: Text}
    deriving stock (Generic, Show)
    deriving newtype (Eq)

data Note = Note
    { noteContent :: Text
    , noteId      :: Text
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromForm)

data Direction = Up | Down
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data Reordering = Reordering
    { reorderingProductId :: Text
    , reorderingDirection :: Direction
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

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
    toHtml (ProductSearchList lang attributes products _rubric listId) =
        div_ [class_ "products", id_ listId] $ do
            mapM_
                ( \p -> div_
                    [class_ "product-container", title_ (productName p)]
                    $ do
                        div_ [class_ "product-image-container"] $
                            img_
                                [ class_ "product-image"
                                , src_ (productImageUrl p)
                                ]
                        div_ [class_ "product-details"] $ do
                            div_ [class_ "product-details-text"] $ do
                                span_ [class_ "product-name"] $
                                    toHtml (productName p)
                                span_ [class_ "product-promo"] $
                                    toHtml $
                                        productPrice p
                                span_ [class_ "product-save"] $
                                    toHtml $
                                        productOffer p
                            button_
                                (class_ "add-to-shopping-list-button" : attributes p)
                                (l_ lang Lexicon.Add)
                )
                products

addToShoppingList :: Language -> Text -> Product -> [Attribute]
addToShoppingList lang grocery p =
    [ HX.hxPost_ (mkHref lang "/inkop/" <> grocery <> "/lagg-till")
    , HX.hxExt_ "json-enc"
    , HX.hxVals_ (TE.decodeUtf8 $ toStrict $ encode p)
    , HX.hxSwap_ "none"
    ]

data ShoppingPage = ShoppingPage
    { spLanguage     :: !Language
    , spProducts     :: ![Product]
    , spGrocery      :: !Text
    , spPromotions   :: ![Product]
    , spShoppingList :: !(Maybe [ShoppingItem])
    }
    deriving stock (Show, Eq)

instance ToHtml ShoppingPage where
    toHtml (ShoppingPage lang products grocery promotions shoppingList) = baseTemplate lang $ do
        h1_ [class_ "shopping-page-title"] $ do
            l_ lang Lexicon.WeeksShoppingList
            div_ $ do
                a_ ([href_ $ mkHref lang "/inkop/willys"] <> if grocery == "willys" then [class_ "link-active"] else []) "Willys" <> " "
                a_ ([href_ $ mkHref lang "/inkop/ica"] <> if grocery == "ica" then [class_ "link-active"] else []) "Ica"
        div_ [class_ "sticky-tabs"] $ do
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
                div_ [class_ "tab-shadow"] ""

        div_ [id_ "product-search-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.SearchAndAddProduct)
            form_ [class_ "gapped-form"] $
                productSearch_ lang mempty (mkHref lang "/inkop/" <> grocery <> "/produkter") products
        div_ [id_ "promotions-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.WeeksOffers)
            toHtml $
                ProductSearchList
                    lang
                    (addToShoppingList lang grocery)
                    (coerce promotions)
                    (l lang Lexicon.Offers)
                    "promotion-products"
        div_ [id_ "shopping-list-container", class_ "tabcontent"] $ do
            h2_ (l_ lang Lexicon.YourShoppingList)
            div_ [class_ "shopping-list-buttons"] $ do
                button_
                    [ class_ "remove-all-button"
                    , type_ "button"
                    , HX.hxDelete_ $ mkHref lang "/inkop/" <> grocery <> "/ta-bort-alla"
                    , HX.hxSwap_ "none"
                    ]
                    (l_ lang Lexicon.RemoveAll)
                button_
                    [ class_ "remove-checked-button"
                    , type_ "button"
                    , HX.hxDelete_ $ mkHref lang "/inkop/" <> grocery <> "/ta-bort"
                    , HX.hxSwap_ "none"
                    ]
                    (l_ lang Lexicon.RemoveMarked)
            case shoppingList of
                Nothing -> p_ (l_ lang Lexicon.SomethingWentWrong)
                Just list -> do
                    div_
                        [ id_ "shopping-list"
                        , HX.hxExt_ "sse"
                        , hxSseConnect_ (mkHref lang "/inkop/" <> grocery <> "/sse")
                        , hxSseSwap_ grocery
                        ]
                        $ toHtml (ShoppingItems lang grocery list)
    toHtmlRaw = toHtml

data Checkbox = Checked | Unchecked
    deriving stock (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

data ShoppingItem = ShoppingItem
    { siProduct :: !Product
    , siCheck   :: !Checkbox
    , siNote    :: !Text
    }
    deriving stock (Generic, Show, Eq)
    deriving (FromJSON, ToJSON) via StripAndLower "si" ShoppingItem

-- instance ToHtml ShoppingItem where
--     toHtml item = shoppingItem_ item
--     toHtmlRaw = toHtml

data ShoppingItems = ShoppingItems !Language !Text ![ShoppingItem]
    deriving stock (Generic, Show, Eq)

instance ToHtml ShoppingItems where
    toHtmlRaw = toHtml
    toHtml (ShoppingItems lang grocery items)
        | null items = p_ (l_ lang Lexicon.NoItems)
        | otherwise = div_ [id_ "shopping-list-items", class_ "bordered"] $ mapM_ (shoppingItem_ lang grocery) items

shoppingItem_ :: (Monad m) => Language -> Text -> ShoppingItem -> HtmlT m ()
shoppingItem_ lang grocery item = div_ [class_ "shopping-item-container", id_ divId] $ do
    div_ [class_ "shopping-item-info"] $ do
        div_ [class_ "shopping-item-reorder-container"] $ do
            button_
                [ class_ "reorder-button"
                , HX.hxPatch_ (mkHref lang "/inkop/" <> grocery <> "/flytta")
                , HX.hxExt_ "json-enc"
                , HX.hxVals_ (TL.toStrict $ encodeToLazyText (Reordering (productId (siProduct item)) Up))
                ]
                "⬆️"
            button_
                [ class_ "reorder-button"
                , HX.hxPatch_ (mkHref lang "/inkop/" <> grocery <> "/flytta")
                , HX.hxExt_ "json-enc"
                , HX.hxVals_ (TL.toStrict $ encodeToLazyText (Reordering (productId (siProduct item)) Down))
                ]
                "⬇️"
        img_ [class_ "item-image", src_ (productImageUrl (siProduct item))]
        div_ [class_ "item-details"] $ do
            div_ [class_ "item-details-text"] $ do
                span_ [class_ "product-name"] $ toHtml (productName (siProduct item))
                span_ [class_ "item-price"] $ toHtml $ productPrice (siProduct item)
                span_ [class_ "item-save"] $
                    toHtml $
                        productOffer (siProduct item)
            input_ $
                [ class_ "item-checkbox"
                , type_ "checkbox"
                , id_ (productId (siProduct item))
                , name_ "name"
                , HX.hxPost_ (mkHref lang "/inkop/" <> grocery <> "/toggla")
                , HX.hxExt_ "json-enc"
                , HX.hxVals_ (TL.toStrict $ encodeToLazyText (siProduct item))
                , autocomplete_ "off"
                ]
                    <> if (siCheck item) == Checked then [checked_] else []
    form_ [class_ "shopping-item-note-container", autocomplete_ "off"] $ do
        input_
            [ class_ "item-note"
            , type_ "text"
            , name_ "noteContent"
            , value_ (siNote item)
            , HX.hxPatch_ (mkHref lang "/inkop/" <> grocery <> "/anteckna")
            , placeholder_ (l lang Lexicon.Note)
            ]
        input_ [type_ "hidden", name_ "noteId", value_ (productId (siProduct item))]
        button_
            [ type_ "submit"
            , HX.hxSwap_ "none"
            , HX.hxPatch_ (mkHref lang "/inkop/" <> grocery <> "/anteckna")
            ]
            (l_ lang Lexicon.Save)
  where
    divId = "shopping-item-" <> productId (siProduct item)

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
