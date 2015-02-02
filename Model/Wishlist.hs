module Model.Wishlist where

import           ClassyPrelude.Yesod
import           Data.List             (nub)
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3, withPlaceholder)

import           Foundation
import           Model
import           Types                 (Priority)
import           Util.Fields           (bfsText, isbnField)


-- Retrieve the Books & WishlistItems of a Wishlist.
getBooksInWishlist :: WishlistId -> Handler [(Entity Book, Entity WishlistItem)]
getBooksInWishlist wishlistId = do
    items  <- runDB $ selectList
              [WishlistItemWishlist ==. wishlistId] []
    mapM (\i@(Entity _ item) -> do
            book <- runDB $ getJust $ wishlistItemBook item
            return (Entity (wishlistItemBook item) book, i)
        ) items

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
    where nameSettings = withPlaceholder "Wishlist Name" $ bfsText "Name"

-- | A Form for validating creation of WishlistItems from only an ISBN &
-- Priorty.
wishlistItemForm :: Form (Text, Priority)
wishlistItemForm       = renderBootstrap3 BootstrapInlineForm $ (,)
    <$> areq isbnField isbnSettings Nothing
    <*> areq (selectField optionsEnum) (bfsText "Priority") Nothing
    where isbnSettings = withPlaceholder "ISBN" $ bfsText "ISBN"
