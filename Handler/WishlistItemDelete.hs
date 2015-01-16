module Handler.WishlistItemDelete where

import           Import


-- | Delete the given WishlistItem.
getWishlistItemDeleteR :: WishlistId -> BookId -> Handler Html
getWishlistItemDeleteR listId bookId = do
    list <- runDB $ getJust listId
    runDB . deleteBy $ WishlistBook listId bookId
    setMessage "Removed Book from Wishlist"
    redirect $ WishlistR $ wishlistName list
