module Handler.Wishlist where

import           Import

import           Text.Julius (juliusFile, rawJS)

import           Types       (Priority (Medium))


-- | Show the WishlistItems & a form to add Books.
getWishlistR :: Text -> Handler Html
getWishlistR name = do
    (wishlistId, wishlist, booksAndItems, otherLists) <- getStandardWishlistData name
    (addBookWidget, addBookEnctype) <- generateFormPost wishlistItemForm
    defaultLayout $ do
        setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
        toWidget $(juliusFile "templates/wishlistGet.julius")
        $(widgetFile "wishlist")

-- | Process the addition of new Books to the Wishlist.
postWishlistR :: Text -> Handler Html
postWishlistR name = do
    (wishlistId, wishlist, booksAndItems, otherLists) <- getStandardWishlistData name
    ((result, addBookWidget), addBookEnctype)   <- runFormPost wishlistItemForm
    case result of
        FormSuccess formInstance -> do
            bookid <- fromMaybe (error "create failed") <$>
                      createBookFromIsbn (fst formInstance)
            mItem  <- runDB $ getBy $ WishlistBook wishlistId bookid
            case mItem of
                 Just _  -> setMessage "That Book is already in your Wishlist"
                 Nothing -> runDB (insert $ WishlistItem bookid wishlistId
                                            $ snd formInstance)
                         >> setMessage "Added Book to your Wishlist"
            redirect $ WishlistR name
        _                -> defaultLayout $ do
            setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
            $(widgetFile "wishlist")


-- | Retrieve variables used in both GET & POST requests
getStandardWishlistData :: Text
                        -> Handler (Key Wishlist, Wishlist,
                                    [(Entity Book, Entity WishlistItem)],
                                    [Entity Wishlist])
getStandardWishlistData name = do
    Entity wishlistId wishlist <- runDB . getBy404 $ UniqueWishlistName name
    booksAndItems              <- getBooksInWishlist wishlistId
    otherLists                 <- runDB $ selectList [WishlistName !=. name] []
    return (wishlistId, wishlist, booksAndItems, otherLists)
