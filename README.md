# MyBookList

MyBookList is a web application written in Haskell that allows you to track
your book library and wishlists.

This project is in early-alpha: currently only a basic library & wishlists are
implemented.


## Motivation

MyBookList was built as an improvement over Amazon's wishlists.

Some improved functionality we would like to incorporate:
* User library for managing owned books.
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
