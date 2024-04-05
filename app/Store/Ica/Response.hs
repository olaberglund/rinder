-- | Encoding the API documented here: https://github.com/svendahlstrand/ica-api/blob/master/api-referens.md
module Store.Ica.Response () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON, FieldLabelModifier, Rename, StripPrefix)
import Deriving.Aeson qualified
import GHC.Base (Symbol)
import GHC.Generics (Generic)

data Offer = Offer
    { offerId :: Text -- OfferId
    , offerStoreId :: [Int] -- StoreId
    , offerArticleGroup :: [Int] -- ArticleGroup
    , offerType :: Text -- OfferType
    , offerImageUrl :: Text -- ImageUrl
    , offerArticles :: [Article] -- Articles
    , offerCondition :: Text -- OfferCondition
    }
    deriving stock (Show, Generic, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[ FieldLabelModifier
                    '[ Rename "offerId" "OfferId"
                     , Rename "offerType" "OfferType"
                     , Rename "offerCondition" "OfferCondition"
                     , StripPrefix "offer"
                     ]
                 ]
                Offer

data Article = Article
    { articleId :: Text -- EanId
    , articleDescription :: Text -- ArticleDescription
    }
    deriving stock (Show, Generic, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[ FieldLabelModifier
                    '[ Rename "articleId" "EanId"
                     , Rename "articleDescription" "ArticleDescription"
                     ]
                 ]
                Article

data Response a (b :: Symbol) = Response
    { unResponse :: a
    }
    deriving stock (Show, Generic, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[ FieldLabelModifier
                    '[ Rename "unResponse" b
                     ]
                 ]
                (Response a b)

type ResponseJSON a b =
    CustomJSON '[FieldLabelModifier '[Rename "unResponse" a]] b

type ItemResponse = Response [Item] "Items"

type OfferResponse = Response [Offer] "Offers"

data Item = Item
    { itemDescription :: Text -- ItemDescription
    , itemArticleGroup :: Int -- ArticleGroup
    , itemArticleGroupExtended :: Int -- ArticleGroupExtended
    }
    deriving stock (Show, Generic, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[ FieldLabelModifier
                    '[ Rename "itemDescription" "ItemDescription"
                     , StripPrefix "item"
                     ]
                 ]
                Item
