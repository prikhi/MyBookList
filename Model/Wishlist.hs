module Model.Wishlist where

import ClassyPrelude.Yesod
import Data.List                (nub)
import Yesod.Form.Bootstrap3    (BootstrapFormLayout(..), renderBootstrap3,
                                 bfs, withPlaceholder)

import Foundation
import Model
import Util.Fields              (isbnField)
import Types                    (Priority)


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


-- | A Form for valdating the creation of new Wishlists.
wishlistForm :: Form Wishlist
wishlistForm           = renderBootstrap3 BootstrapInlineForm $ Wishlist
    <$> areq textField nameSettings Nothing
    where nameSettings = withPlaceholder "Wishlist Name" $ bfs ("Name" :: Text)

-- | A Form for validating creation of WishlistItems from only an ISBN &
-- Priorty.
wishlistItemForm :: Form (Text, Priority)
wishlistItemForm       = renderBootstrap3 BootstrapInlineForm $ (,)
    <$> areq isbnField isbnSettings Nothing
    <*> areq (selectField optionsEnum) (bfs ("Priority" :: Text)) Nothing
    where isbnSettings = withPlaceholder "ISBN" $ bfs ("ISBN" :: Text)
