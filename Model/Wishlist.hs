module Model.Wishlist where

import ClassyPrelude.Yesod
import Foundation

import Model

getBooksInWishlist :: WishlistId -> Handler [(BookId, Book)]
getBooksInWishlist wishlistId = do
    items  <- runDB $ selectList
              [WishlistItemWishlist ==. wishlistId] []
    mBooks <- liftM sequence $ mapM (\(Entity _ i) -> do
                   let bookId = wishlistItemBook i
                   book <- runDB $ getJust bookId
                   return $ Just (bookId, book)
              ) items
    return $ fromMaybe [] mBooks
