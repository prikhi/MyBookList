module Handler.WishlistItemDelete where

import           Data.Maybe (fromJust)
import           Import


-- | Delete the given WishlistItem.
getWishlistItemDeleteR :: WishlistId -> BookId -> Handler Html
getWishlistItemDeleteR listId bookId = do
    mUserId         <- maybeAuthId
    when (isNothing mUserId) notFound
    let userId       = fromJust mUserId
    list            <- runDB $ getJust listId
    when (userId /= wishlistUser list) notFound
    runDB . deleteBy $ WishlistBook listId bookId
    setMessage "Removed Book from Wishlist"
    slug            <- getSlug userId
    redirect $ WishlistR slug $ wishlistName list
    where getSlug userId = do
           Entity _ profile <- runDB . getBy404 $ UniqueProfile userId
           return $ userProfileSlug profile
