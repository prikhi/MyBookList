module Model.Wishlist where

import ClassyPrelude.Yesod
import Data.List             (nub)
import Foundation

import Model

-- Retrieve the BookIds & Books of a Wishlist.
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

-- Return the 5 highest priority Books in all Wishlists.
getMostWantedBooks :: Handler [Book]
getMostWantedBooks = runDB $ do
    items     <- selectList [] []
    let books = take 5 . nub . map entityBook .
                sortBy (flip compare `on` entityPriority) $ items
    mapM getJust books
    where entityPriority (Entity _ i) = wishlistItemPriority i
          entityBook (Entity _ i)     = wishlistItemBook i
