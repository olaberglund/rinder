module Store.Ica.Response () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON, FieldLabelModifier, Rename, StripPrefix)
import Deriving.Aeson qualified
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
                    '[ StripPrefix "offer"
                     , Rename "Id" "OfferId"
                     , Rename "Type" "OfferType"
                     , Rename "Condition" "OfferCondition"
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
