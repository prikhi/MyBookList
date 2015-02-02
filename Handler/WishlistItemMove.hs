module Handler.WishlistItemMove where

import           Import

getWishlistItemMoveR :: Text -> WishlistItemId -> Handler Html
getWishlistItemMoveR name wishlistItemId = do
    Entity listId list <- runDB $ getBy404 $ UniqueWishlistName name
    _    <- runDB $ update wishlistItemId [WishlistItemWishlist =. listId]
    _    <- setMessage $ "Moved Book to " `mappend` toHtml name
    redirect . WishlistR $ wishlistName list
