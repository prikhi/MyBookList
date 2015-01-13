module Handler.WishlistDelete where

import Import

getWishlistDeleteR :: WishlistId -> BookId -> Handler Html
getWishlistDeleteR listId bookId = do
    list <- runDB $ getJust listId
    runDB . deleteBy $ WishlistBook listId bookId
    setMessage "Removed Book from Wishlist"
    redirect $ WishlistR $ wishlistName list
