{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import           Import


-- | Show a dashboard containing an overview of wishlists & the library.
getHomeR :: Handler Html
getHomeR  = do
    ( wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
      libraryWidget, libraryEnctype)  <- getStandardHomeData
    (wishlistWidget, wishlistEnctype) <- generateFormPost wishlistForm
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

-- | Process the Create Wishlist form.
postHomeR :: Handler Html
postHomeR = do
    ( wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
      libraryWidget, libraryEnctype)            <- getStandardHomeData
    ((result, wishlistWidget), wishlistEnctype) <- runFormPost wishlistForm
    case result of
         FormSuccess wishlist -> runDB (insert wishlist) >>
                                 setMessage "Successfully added Wishlist" >>
                                 redirect HomeR
         _                    -> setMessage "Encountered an error while creating Wishlist"
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")


-- | Retrieve standard variables used in both GET & POST requests.
getStandardHomeData :: Handler ( [Entity Wishlist], [Book], [Book], [Book]
                               , [Book], Widget, Enctype )
getStandardHomeData = do
    wishlists        <- runDB $ selectList [] []
    wanted           <- getMostWantedBooks
    inProgress       <- map snd <$> getInProgressBooks
    recentlyFinished <- map snd <$> getRecentlyFinishedBooks
    newlyAdded       <- map snd <$> getNewlyAddedBooks
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemIsbnForm
    return (wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
            libraryWidget, libraryEnctype)
