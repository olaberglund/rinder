module Server.Split.Html (
    EditExpensePage (..),
    Transactions (..),
    SplitPage (..),
    FeedbackMessage (FeedbackMessage),
    Feedback (Success),
)
where

import qualified Data.List         as List
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Inter.Language    (Language, mkHref)
import           Inter.Lexicon     (l, l_)
import qualified Inter.Lexicon     as Lexicon
import           Lucid
import qualified Lucid.Base
import           Numeric           (fromRat, showFFloat)
import           Safe              (headMay)
import           Server.Utils.Html (baseTemplate, text)
import           Split

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
                            select_ [name_ "debtor"] $
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
                                , id_ "value"
                                , name_ "value"
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
                        , hxPost_ (mkHref lang "/split/lagg-till")
                        , hxTarget_ "#tally-expenses-container"
                        , hxSwap_ "outerHTML"
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
                        , hxPost_ (mkHref lang "/split/gor-upp")
                        , hxTarget_ "#tally-expenses-container"
                        , hxSwap_ "outerHTML"
                        ]
                        (l_ lang Lexicon.Settle)
            h2_ (l_ lang Lexicon.Expenses)
            if null transactions
                then p_ (l_ lang Lexicon.NoExpenses)
                else div_ [class_ "expenses-container"] $ mapM_ (toHtml . TransactionHtml lang) transactions
      where
        settles = simplifiedDebts transactions

iou_ :: (Monad m) => Language -> (Person, [(Person, Amount)]) -> HtmlT m ()
iou_ lang (p, ious') = do
    span_
        [ class_ "creditor-name"
        , style_ $ "background-color: " <> personColor p
        ]
        $ toHtml
        $ personName p <> " " <> l lang Lexicon.IsOwed
    mapM_ (debtItem_ lang) ious'

debtItem_ :: (Monad m) => Language -> (Person, Amount) -> HtmlT m ()
debtItem_ lang (p, amount) =
    toHtml $ personName p <> ": " <> showAmount amount <> " " <> l lang Lexicon.Currency

data EditExpensePage
    = EditExpensePage
        !Language
        !Expense
        !Share
        !(Maybe FeedbackMessage)

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
                        select_ [name_ "debtor"] $
                            mapM_
                                ( \p ->
                                    option_
                                        ( [value_ (personName p)]
                                            <> ([selected_ "selected" | p == sharePerson debtorShare])
                                        )
                                        (toHtml (personName p))
                                )
                                (peopleOfExpense e)
                        span_ [class_ "form-comment"] (l_ lang Lexicon.Pays)
                    div_ [class_ "debtor-form-group"] $ do
                        input_
                            [ type_ "number"
                            , id_ "value"
                            , name_ "value"
                            , value_ (showShare debtorShare)
                            ]
                        select_
                            [ name_ "share-type"
                            , value_ (showShare debtorShare)
                            ]
                            $ mapM_
                                ( \st ->
                                    option_
                                        ( [value_ (shareTypeToText st)]
                                            <> ([selected_ "selected" | st == valueType (shareValue debtorShare)])
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
                            , value_ (showAmount $ expenseTotal e)
                            ]
                        span_ $ l_ lang Lexicon.Currency
                        span_ "*"
                small_ (l_ lang Lexicon.RestIsPaidByOther)
                div_ [id_ "edit-action-buttons"] $ do
                    button_
                        [ type_ "submit"
                        , hxPatch_ (mkHref lang $ "/split/spara/" <> text (expenseId e))
                        , hxTarget_ "body"
                        ]
                        (l_ lang Lexicon.Save)
                    button_
                        [ type_ "button"
                        , hxDelete_ (mkHref lang $ "/split/ta-bort/" <> text (expenseId e))
                        , hxSwap_ "none"
                        ]
                        (l_ lang Lexicon.Remove)
                maybe mempty toHtml message

-- | Convert a share type to text. Useful for use in name attributes in forms.
shareTypeToText :: ShareType -> Text
shareTypeToText Percentage = "percentage"
shareTypeToText Fixed      = "fixed"

-- | Symbol for share type. Useful for displaying the share type in the UI.
shareTypeSymbol :: Language -> ShareType -> Text
shareTypeSymbol _ Percentage = "%"
shareTypeSymbol lang Fixed   = l lang Lexicon.Currency

data TransactionHtml = TransactionHtml !Language !Transaction

instance ToHtml TransactionHtml where
    toHtmlRaw = toHtml
    toHtml (TransactionHtml lang (ExpenseTransaction e)) =
        div_ [class_ "expense-container"] $ do
            div_ [class_ "expense-info-container"] $ do
                h3_ [class_ "expense-title", title_ (expenseRubric e)] $ toHtml $ expenseRubric e
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
                        showAmount (expenseTotal e)
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
                        showAmount (settlementAmount s)
                            <> " "
                            <> l lang Lexicon.Currency
                i_ [class_ "settle-icons"] "💸"

split_ :: (Monad m) => Expense -> HtmlT m ()
split_ expense = div_ [class_ "split-container"] $ pieChart_ expense 50

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
    colorShare :: Text -> Rational -> Text
    colorShare col rat = col <> " " <> Text.pack (showFFloat (Just 2) (fromRat (100 * rat) :: Float) "%")

    colorShares :: Amount -> [Share] -> Text
    colorShares total =
        Text.intercalate ", "
            . snd
            . List.foldl' (genColorText total) (0, [])

    genColorText :: Amount -> (Rational, [Text]) -> Share -> (Rational, [Text])
    genColorText _ (sum', txt) (Share (Value Percentage pct) p _) =
        ( sum' + pct
        , txt
            <> [ colorShare (personColor p) sum'
               , colorShare (personColor p) (sum' + pct)
               ]
        )
    genColorText total (sum', txt) (Share val p _) =
        ( sum' + toPercent val
        , txt
            <> [ colorShare (personColor p) sum'
               , colorShare (personColor p) (sum' + toPercent val)
               ]
        )
      where
        toPercent :: Value -> Rational
        toPercent = (/ toRational total) . value

hxSwap_ :: Text -> Attribute
hxSwap_ = Lucid.Base.makeAttribute "hx-swap"

hxPatch_ :: Text -> Attribute
hxPatch_ = Lucid.Base.makeAttribute "hx-patch"

hxPost_ :: Text -> Attribute
hxPost_ = Lucid.Base.makeAttribute "hx-post"

hxTarget_ :: Text -> Attribute
hxTarget_ = Lucid.Base.makeAttribute "hx-target"

hxDelete_ :: Text -> Attribute
hxDelete_ = Lucid.Base.makeAttribute "hx-delete"
