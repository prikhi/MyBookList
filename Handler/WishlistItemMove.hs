module Handler.WishlistItemMove where

import           Data.Maybe (fromJust)
import           Import

getWishlistItemMoveR :: Text -> WishlistItemId -> Handler Html
getWishlistItemMoveR name wishlistItemId = do
    mUserId            <- maybeAuthId
    when (isNothing mUserId) notFound
    let userId          = fromJust mUserId
    Entity listId list <- runDB $ getBy404 $ UniqueWishlist name userId
    when (userId /= wishlistUser list) notFound
    _    <- runDB $ update wishlistItemId [WishlistItemWishlist =. listId]
    _    <- setMessage $ "Moved Book to " `mappend` toHtml name
    slug               <- getSlug userId
    redirect . WishlistR slug $ wishlistName list
    where getSlug userId = do
           Entity _ profile <- runDB . getBy404 $ UniqueProfile userId
           return $ userProfileSlug profile
