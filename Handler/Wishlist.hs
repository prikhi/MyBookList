module Handler.Wishlist where


import           Text.Julius  (juliusFile, rawJS)

import           Import
import           Types        (Priority (..))
import           Util.Widgets (getSortValue, sortWidget, sortListByOption)

-- | A 'Book' 'Entity' & 'WishlistItem' 'Entity' packed into a Tuple.
type WishlistItemAndBook = (Entity Book, Entity WishlistItem)

-- | Show the WishlistItems & a form to add Books.
getWishlistR :: Text -> Handler Html
getWishlistR name = do
    (wishlistId, wishlist, booksAndItems, otherLists) <- getStandardWishlistData name
    (addBookWidget, addBookEnctype) <- generateFormPost wishlistItemForm
    defaultLayout $ do
        setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
        toWidget $(juliusFile "templates/wishlist/wishlistGet.julius")
        $(widgetFile "wishlist/wishlist")

-- | Process the addition of new Books to the Wishlist.
postWishlistR :: Text -> Handler Html
postWishlistR name = do
    (wishlistId, wishlist, booksAndItems, otherLists) <- getStandardWishlistData name
    ((result, addBookWidget), addBookEnctype)   <- runFormPost wishlistItemForm
    case result of
        FormSuccess formInstance -> do
            bookid       <- fromMaybe (error "create failed") <$>
                            createBookFromIsbn (fst formInstance)
            mLibraryItem <- runDB $ getBy $ UniqueLibraryBook bookid
            when (isJust mLibraryItem) $ do
                setMessage "That book is already in your Library"
                redirect $ WishlistR name
            mItem        <- runDB $ getBy $ WishlistBook wishlistId bookid
            case mItem of
                 Just _  -> setMessage "That Book is already in your Wishlist"
                 Nothing -> runDB (insert $ WishlistItem bookid wishlistId
                                          $ snd formInstance)
                         >> setMessage "Added Book to your Wishlist"
            redirect $ WishlistR name
        _                        -> defaultLayout $ do
            setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
            $(widgetFile "wishlist/wishlist")


-- | Retrieve variables used in both GET & POST requests
getStandardWishlistData :: Text
                        -> Handler (Key Wishlist, Wishlist,
                                    [WishlistItemAndBook], [Entity Wishlist])
getStandardWishlistData name = do
    sortVal            <- getSortValue "priority"
    Entity listId list <- runDB . getBy404 $ UniqueWishlistName name
    booksAndItems      <- sortWishlist sortVal <$> getBooksInWishlist listId
    otherLists         <- runDB $ selectList [WishlistName !=. name] []
    return (listId, list, booksAndItems, otherLists)
    where sortWishlist       = sortListByOption wishlistSortingOptions


-- | Render a dropdown button group for modifying a 'WishlistItemPriority'.
priorityDropdownWidget :: WishlistItemId -> Priority -> Text -> Widget
priorityDropdownWidget itemId priority btnColorClass =
    let otherPriorities = filter (/= priority)
                                 [Highest, High, Medium, Low, Lowest]
    in  $(widgetFile "wishlist/priorityDropdown")


-- | Display a Dropdown Button to Select a Wishlist Sorting Option.
wishlistSortWidget :: Widget
wishlistSortWidget = sortWidget wishlistSortingOptions


-- | Return the GET Value, Name & Sorting Function for Library Sort Types.
wishlistSortingOptions :: [(Text, Text,
                            WishlistItemAndBook -> WishlistItemAndBook ->
                            Ordering)]
wishlistSortingOptions =
    [ ("priority", "Priority", flipComparing $ wishlistItemPriority . getItem)
    , ("name", "Name (A-Z)", comparing $ bookTitle . getBook)
    , ("name-reverse", "Name (Z-A)", flipComparing $ bookTitle . getBook)
    ]
    where getBook (Entity _ b, _) = b
          getItem (_, Entity _ i) = i
