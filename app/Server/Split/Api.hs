module Server.Split.Api (SplitApi (..)) where

import           Data.UUID          (UUID)
import           GHC.Generics       (Generic)
import           Servant            (Capture, Delete, FormUrlEncoded,
                                     GenericMode (type (:-)), Get,
                                     NoContent (..), Patch, Post, ReqBody,
                                     type (:>))
import           Servant.HTML.Lucid (HTML)
import           Server.Split.Html
import           Split              (ExpenseForm)

data SplitApi as = SplitApi
    { splitPageEP :: !(as :- Get '[HTML] SplitPage)
    , settleUpEP :: !(as :- "gor-upp" :> Post '[HTML] Transactions)
    , removeExpenseEp ::
        !( as
            :- "ta-bort"
                -- \| The UUID of the expense to remove
                :> Capture "id" UUID
                -- \| Redirects upon successfull deletion
                :> Delete '[HTML] NoContent
         )
    , newExpenseEP ::
        !( as
            :- "lagg-till"
                -- \| Create a new expense from the expense form
                :> ReqBody '[FormUrlEncoded] ExpenseForm
                -- \| Swaps in the list of transactions, including the new expense
                :> Post '[HTML] Transactions
         )
    , editExpensePageEP ::
        !( as
            :- "redigera"
                -- \| The UUID of the expense to edit
                :> Capture "id" UUID
                :> Get '[HTML] EditExpensePage
         )
    , saveExpenseEP ::
        !( as
            :- "spara"
                -- \| The UUID of the expense to edit
                :> Capture "id" UUID
                -- \| The updated expense
                :> ReqBody '[FormUrlEncoded] ExpenseForm
                -- \| Updated expense page with feedback and updated expense details
                :> Patch '[HTML] EditExpensePage
         )
    }
    deriving stock (Generic)
