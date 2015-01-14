# MyBookList

MyBookList is a web application written in Haskell that allows you to track
your book library and wishlists.

This project is in early-alpha. Currently only wishlists are implemented.

Feature requests welcome.

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
