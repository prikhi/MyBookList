module Handler.Library where

import           Import
import           Util.Widgets (getSortValue, sortListByOption, sortWidget)


-- | A 'LibraryItem' 'Entity' & 'Book' packed into a Tuple.
type LibraryItemAndBook = (Entity LibraryItem, Book)


-- | Display all LibraryItems.
getLibraryR :: Text -> Handler Html
getLibraryR userSlug = do
    Entity _ profile   <- runDB $ getBy404 $ UniqueSlug userSlug
    Entity libraryId _ <- runDB $ getBy404 $ UserLibrary $ userProfileUser profile
    items   <- runDB $ selectList [LibraryItemLibrary ==. libraryId] []
    sortVal <- getSortValue "status"
    books   <- mapM (\(Entity _ i) -> runDB $ getJust $ libraryItemBook i) items
    let itemsAndBooks = sortLibrary sortVal $ zip items books
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemIsbnForm
    defaultLayout $ do
        setTitle "Library"
        $(widgetFile "library/library")
    where sortLibrary       = sortListByOption librarySortingOptions
          librarySortWidget = sortWidget librarySortingOptions


-- | Return the GET Value, Name & Sorting Function for Library Sort Types.
librarySortingOptions :: [(Text, Text,
                           LibraryItemAndBook -> LibraryItemAndBook -> Ordering)]
librarySortingOptions =
    [ ("status", "Status", sortStatus `on` getItem)
    , ("rating", "Rating", comparing $ libraryItemRating . getItem)
    , ("name", "Name (A-Z)", flipComparing $ bookTitle . snd)
    , ("name-reverse", "Name (Z-A)", comparing $ bookTitle . snd)
    , ("newest", "Newest", comparing $ libraryItemAddedOn . getItem)
    , ("oldest", "Oldest", flipComparing $ libraryItemAddedOn . getItem)
    , ("recently-finished", "Recently Finished", comparing $ libraryItemLastFinishedOn . getItem)
    , ("times-finished", "Times Finished", comparing $ libraryItemCompletionCount . getItem)
    ]
    where getItem (Entity _ i, _) = i
          sortStatus item1 item2  =
                let result        = comparing libraryItemInProgress item1 item2
                 in if result == EQ
                        then flipComparing libraryItemHasFinished item1 item2
                        else result
