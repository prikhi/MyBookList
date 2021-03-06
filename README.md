# MyBookList

MyBookList is a web application that allows you to track your book library and
wishlists.

This project is still in alpha development: new features are consistently being
adding, the API & database schema is volatile, some bugs may exist & some
expected functionality might be mising. Some things you may miss:
* New & Used Prices for Wishlist Items
* Moving wishlist items to the library
* Statistics
* User Accounts(only a single user per database)


## Motivation

MyBookList was built as an improvement over Amazon's wishlists & a way to
manage and track your progress through your library.

Some improved functionality we would like to incorporate:
* Pull new/used prices for books in wishlists.
* Sort wishlist books by either used or new price.
* Notifications for low prices on watched books.
* Book clubs with discussions.
* Recommend books to friends.
* Statistics, graphs & charts based on reading & purchasing history.

Additional feature requests welcome.


## Install

Grab the source:

    git clone http://bugs.sleepanarchy.com/mybooklist.git
    cd mybooklist

Create a sandbox:

    cabal sandbox init

Install dependencies:

    cabal install --only-dependencies --max-backjumps=-1 -j

Run development server:

    yesod devel

You should now be able to visit your local MyBookList at http://localhost:3000.


## Contributing

The site is written in Haskell, on top of the Yesod web framework.

Add models to `config/models`, routes to `config/routes`, handlers to
`Handlers` & model functionality to the appropriate `Model` sub-module.

Please reference surrounding code for style preferences.

Source code, bug reports & feature requests:
http://bugs.sleepanarchy.com/projects/mybooklist/
http://github.com/prikhi/mybooklist/
