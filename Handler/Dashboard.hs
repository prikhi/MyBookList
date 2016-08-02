{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Dashboard where

import           Data.Maybe (fromJust)

import           Import


-- | Show a dashboard containing an overview of wishlists & the library.
getDashboardR :: Handler Html
getDashboardR  = do
    ( wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
      libraryWidget, libraryEnctype, userSlug, userId) <- getStandardDashboardData
    (wishlistWidget, wishlistEnctype)          <- generateFormPost $ wishlistForm userId
    defaultLayout $ do
        setTitle "Your Dashboard"
        $(widgetFile "dashboard")

-- | Process the Create Wishlist form.
postDashboardR :: Handler Html
postDashboardR = do
    ( wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
      libraryWidget, libraryEnctype, userSlug, userId)  <- getStandardDashboardData
    ((result, wishlistWidget), wishlistEnctype) <- runFormPost $ wishlistForm userId
    case result of
         FormSuccess wishlist ->
            runDB (getBy $ UniqueWishlist (wishlistName wishlist) userId) >>=
            maybe (runDB (insert wishlist) >>
                   setMessage "Successfully added Wishlist")
                  (const $ setMessage "This Wishlist already exists") >>
            redirect (WishlistR userSlug $ wishlistName wishlist)
         _                    ->
            setMessage "Encountered an error while creating Wishlist" >>
            defaultLayout (setTitle "Welcome To MyBookList!" >>
                           $(widgetFile "dashboard"))


-- | Retrieve standard variables used in both GET & POST requests.
getStandardDashboardData :: Handler ( [Entity Wishlist], [Book], [Book], [Book]
                               , [Book], Widget, Enctype, Text, UserId )
getStandardDashboardData = do
    whenM (isNothing <$> maybeAuthId) notFound
    wishlists        <- runDB $ selectList [] []
    wanted           <- getMostWantedBooks
    inProgress       <- map snd <$> getInProgressBooks
    recentlyFinished <- map snd <$> getRecentlyFinishedBooks
    newlyAdded       <- map snd <$> getNewlyAddedBooks
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemIsbnForm
    userProfile      <- fromJust <$> getProfile
    let userSlug     = getSlug userProfile
        userId       = getId userProfile
    return (wishlists, wanted, inProgress, recentlyFinished, newlyAdded,
            libraryWidget, libraryEnctype, userSlug, userId)
    where getSlug (_, Entity _ profile) = userProfileSlug profile
          getId (Entity userId _, _)    = userId
