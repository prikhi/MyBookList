{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import Import


-- | Show a dashboard containing an overview of wishlists & the library.
getHomeR :: Handler Html
getHomeR = do
    wishlists <- runDB $ selectList [] []
    wanted    <- getMostWantedBooks
    (wishlistWidget, wishlistEnctype)         <- generateFormPost wishlistForm
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

-- | Process the Create Wishlist form.
postHomeR :: Handler Html
postHomeR = do
    wishlists <- runDB $ selectList [] []
    wanted    <- getMostWantedBooks
    ((result, wishlistWidget), wishlistEnctype) <- runFormPost wishlistForm
    case result of
         FormSuccess wishlist -> do _ <- runDB (insert wishlist)
                                    setMessage "Successfully added Wishlist"
                                    redirect HomeR
         _                    -> setMessage "Encountered an error while creating Wishlist"
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")
