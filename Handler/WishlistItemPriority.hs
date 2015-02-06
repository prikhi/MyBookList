module Handler.WishlistItemPriority where

import           Import

import           Types  (Priority)

getWishlistItemPriorityR :: WishlistItemId -> Priority -> Handler Html
getWishlistItemPriorityR wishlistItemId priority = do
    runDB $ update wishlistItemId [WishlistItemPriority =. priority]
    item <- runDB $ getJust wishlistItemId
    list <- runDB . getJust $ wishlistItemWishlist item
    setMessage $ "Changed item's priority to " `mappend` toHtml (show priority)
    redirect . WishlistR $ wishlistName list
