{- | This module contains the translations and translation logic
for the application (internationalization).
-}
module Inter.Lexicon (Phrase (..), Language (..), l, l_, mkHref, mkApiHref, toHref, flag) where

import           Data.Text      (Text)
import           Inter.Language
import           Lucid          (HtmlT, toHtml)

data Phrase
    = AddExpenseLegend
    | ExpenseRubric
    | Paid
    | Debt
    | RestIsPaidByOther
    | Add
    | Edit
    | Payment
    | Currency
    | Expenses
    | NoExpenses
    | NoDebts
    | Of
    | Pays
    | Save
    | Remove
    | NavbarSplit
    | NavbarShoppingList
    | AddShoppingItem
    | Item
    | Price
    | Quantity
    | Total
    | RemoveAll
    | RemoveMarked
    | ShoppingList
    | Offers
    | Search
    | WeeksOffers
    | SearchAndAddProduct
    | Product
    | SearchForAProduct
    | ShowAll
    | SearchResults
    | IsOwed
    | Settle
    | EditExpense
    | PaymentTo
    | WeeksShoppingList
    | YourShoppingList
    | SomethingWentWrong
    | Show
    | Debts
    | NoSuchExpense
    | ExpenseSaved
    | MaxTwoPeople
    | NoItems
    | Note
    deriving stock (Show)

l_ :: (Monad m) => Language -> Phrase -> HtmlT m ()
l_ lang p = toHtml $ l lang p

l :: Language -> Phrase -> Text
l = flip translate

translate :: Phrase -> Language -> Text
translate = \case
    AddExpenseLegend -> \case
        SE -> "Lägg till utgift"
        EN -> "Add an expense"
        Simlish -> "Boola baboo frish!"
    ExpenseRubric -> \case
        SE -> "Rubrik"
        EN -> "Rubric"
        Simlish -> "Froobla"
    Paid -> \case
        SE -> "Betalade"
        EN -> "Paid"
        Simlish -> "Shwee!"
    Debt -> \case
        SE -> "Skuld"
        EN -> "Debt"
        Simlish -> "Zibbly zib"
    RestIsPaidByOther -> \case
        SE -> "*Resten betalas av den andre"
        EN -> "*Rest is paid by the other"
        Simlish -> "*Woo wooble flim!"
    Add -> \case
        SE -> "Lägg till"
        EN -> "Add"
        Simlish -> "Gibble gabble!"
    Edit -> \case
        SE -> "Redigera"
        EN -> "Edit"
        Simlish -> "Zoop zoop!"
    Payment -> \case
        SE -> "Swish"
        EN -> "Payment"
        Simlish -> "Swibble"
    Currency -> \case
        SE -> "kr"
        EN -> "SEK"
        Simlish -> "Zibby"
    Expenses -> \case
        SE -> "Utgifter"
        EN -> "Expenses"
        Simlish -> "Booble blibb"
    NoExpenses -> \case
        SE -> "Inga utgifter att visa"
        EN -> "No expenses to show"
        Simlish -> "Noofa loofa booba"
    NoDebts -> \case
        SE -> "Inga skulder att visa"
        EN -> "No debts to show"
        Simlish -> "Neeba dooble boop"
    Of -> \case
        SE -> "av"
        EN -> "of"
        Simlish -> "Oofo"
    Pays -> \case
        SE -> "ska betala"
        EN -> "should pay"
        Simlish -> "Zoobly zorp"
    Save -> \case
        SE -> "Spara"
        EN -> "Save"
        Simlish -> "Smeega"
    Remove -> \case
        SE -> "Ta bort"
        EN -> "Remove"
        Simlish -> "Zibble wobble"
    NavbarSplit -> \case
        SE -> "Split"
        EN -> "Split"
        Simlish -> "Nab nub"
    NavbarShoppingList -> \case
        SE -> "Inköpslista"
        EN -> "Shopping list"
        Simlish -> "Noozle nuzzle"
    AddShoppingItem -> \case
        SE -> "Lägg till vara"
        EN -> "Add item"
        Simlish -> "Nibble nubble"
    Item -> \case
        SE -> "Vara"
        EN -> "Item"
        Simlish -> "Nobby"
    Price -> \case
        SE -> "Pris"
        EN -> "Price"
        Simlish -> "Pribbly"
    Quantity -> \case
        SE -> "Antal"
        EN -> "Quantity"
        Simlish -> "Quobbo"
    Total -> \case
        SE -> "Totalt"
        EN -> "Total"
        Simlish -> "Tooby"
    RemoveAll -> \case
        SE -> "Ta bort alla"
        EN -> "Remove all"
        Simlish -> "Ribble wobble noob"
    RemoveMarked -> \case
        SE -> "Ta bort markerade"
        EN -> "Remove marked"
        Simlish -> "Ribble wibble marp"
    ShoppingList -> \case
        SE -> "Inköpslista"
        EN -> "Shopping list"
        Simlish -> "Shnibble shnabble"
    Offers -> \case
        SE -> "Erbjudanden"
        EN -> "Offers"
        Simlish -> "Oobly ooble"
    Search -> \case
        SE -> "Sök"
        EN -> "Search"
        Simlish -> "Swooble"
    WeeksOffers -> \case
        SE -> "Veckans erbjudanden"
        EN -> "This week's offers"
        Simlish -> "Woozle oobly doo"
    SearchAndAddProduct -> \case
        SE -> "Sök och lägg till produkter"
        EN -> "Search and add products"
        Simlish -> "Swabble and shobble produbles"
    Product -> \case
        SE -> "Produkt"
        EN -> "Product"
        Simlish -> "Produb"
    SearchForAProduct -> \case
        SE -> "Sök efter en produkt..."
        EN -> "Search for a product... (Swedish)"
        Simlish -> "Swabble dooble proddle... (Swomple)"
    ShowAll -> \case
        SE -> "Visa"
        EN -> "Show"
        Simlish -> "Shobble ooble"
    SearchResults -> \case
        SE -> "Sökresultat"
        EN -> "Search results"
        Simlish -> "Swabble rupple"
    IsOwed -> \case
        SE -> "är skyldig"
        EN -> "owes"
        Simlish -> "Oobly booble"
    Settle -> \case
        SE -> "Gör upp"
        EN -> "Settle up"
        Simlish -> "Swabble ooble"
    EditExpense -> \case
        SE -> "Redigera utgift"
        EN -> "Edit expense"
        Simlish -> "Zooply ooploob"
    PaymentTo -> \case
        SE -> "Swish till"
        EN -> "Payment to"
        Simlish -> "Swabble oop"
    WeeksShoppingList -> \case
        SE -> "Veckans inköpslista på"
        EN -> "This week's shopping list at"
        Simlish -> "Woozle shnibble shnabble ap"
    YourShoppingList -> \case
        SE -> "Din inköpslista"
        EN -> "Your shopping list"
        Simlish -> "Yobby shnibble shnabble"
    SomethingWentWrong -> \case
        SE -> "Något gick fel"
        EN -> "Something went wrong"
        Simlish -> "Shooby booble zoob"
    Show -> \case
        SE -> "Visa"
        EN -> "Show"
        Simlish -> "Shabble"
    Debts -> \case
        SE -> "Skulder"
        EN -> "Debts"
        Simlish -> "Doobly"
    NoSuchExpense -> \case
        SE -> "Ingen sådan utgift hittades"
        EN -> "No such expense found"
        Simlish -> "Noozle shnabble booble"
    ExpenseSaved -> \case
        SE -> "Utgift sparad"
        EN -> "Expense saved"
        Simlish -> "Booble smeega"
    MaxTwoPeople -> \case
        SE -> "Just nu stöds inte redigering av utgifter med fler än två personer."
        EN -> "Currently, editing expenses with more than two people is not supported."
        Simlish -> "Jibbly nooble dooble zibbly zorp"
    NoItems -> \case
        SE -> "Din inköpslista är tom"
        EN -> "Your shopping list is empty"
        Simlish -> "Yobby shnibble shnabble zibby"
    Note -> \case
        SE -> "Anteckning"
        EN -> "Note"
        Simlish -> "Nooble"
