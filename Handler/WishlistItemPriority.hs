module Handler.WishlistItemPriority where

import           Data.Maybe (fromJust)
import           Import

import           Types  (Priority)

getWishlistItemPriorityR :: WishlistItemId -> Priority -> Handler Html
getWishlistItemPriorityR wishlistItemId priority = do
    item        <- runDB $ getJust wishlistItemId
    list        <- runDB . getJust $ wishlistItemWishlist item
    mUserId     <- maybeAuthId
    when (isNothing mUserId) notFound
    let userId   = fromJust mUserId
    when (userId /= wishlistUser list) notFound
    runDB $ update wishlistItemId [WishlistItemPriority =. priority]
    setMessage $ "Changed item's priority to " `mappend` toHtml (show priority)
    slug            <- getSlug userId
    redirect . WishlistR slug $ wishlistName list
    where getSlug userId = do
           Entity _ profile <- runDB . getBy404 $ UniqueProfile userId
           return $ userProfileSlug profile
