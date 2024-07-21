# Rinder

## Testing

`cabal run`

## Installation/Usage

`cabal install` 

`rinder <port>`

## Features

- Plan this week's shopping list using local grocery store's offers, or search for a product
  - Uses SSE to display the updated shopping list state in real time (useful when splitting up at the grocery store)
- Keep track of expenses. Other apps were too complicated, costly, or showed too many ads.
  - The algorithm for minimization of debt transactions is able to handle multiple people, but the frontend is currently only made for a group of two people.
- Edit expenses
- Internationalization (Swedish/English/Simlish)
