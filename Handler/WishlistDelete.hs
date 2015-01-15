module Handler.WishlistDelete where

import Import


-- | Delete the Wishlist & any related WIshlistItems.
getWishlistDeleteR :: WishlistId -> Handler Html
getWishlistDeleteR wishlistId = do
    list <- runDB $ get404 wishlistId
    runDB $ deleteWhere [WishlistItemWishlist ==. wishlistId] >> delete wishlistId
    setMessage $ "Successfully deleted the " `mappend` toHtml (wishlistName list)
        `mappend` " Wishlist."
    redirect HomeR
