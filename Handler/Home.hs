{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import Import


-- | Show a dashboard containing an overview of wishlists & the library.
getHomeR :: Handler Html
getHomeR      = do
    (wishlists, wanted, libraryWidget, libraryEnctype) <- getStandardHomeData
    (wishlistWidget, wishlistEnctype) <- generateFormPost wishlistForm
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

-- | Process the Create Wishlist form.
postHomeR :: Handler Html
postHomeR     = do
    (wishlists, wanted, libraryWidget, libraryEnctype) <- getStandardHomeData
    ((result, wishlistWidget), wishlistEnctype) <- runFormPost wishlistForm
    case result of
         FormSuccess wishlist -> do _ <- runDB $ insert wishlist
                                    setMessage "Successfully added Wishlist"
                                    redirect HomeR
         _                    -> setMessage "Encountered an error while creating Wishlist"
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

-- | Retrieve standard variables used in both GET & POST requests.
getStandardHomeData :: Handler ([Entity Wishlist], [Book], Widget, Enctype)
getStandardHomeData = do
    wishlists <- runDB $ selectList [] []
    wanted    <- getMostWantedBooks
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemForm
    return (wishlists, wanted, libraryWidget, libraryEnctype)
