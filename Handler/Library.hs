module Handler.Library where

import           Data.Maybe (fromJust)

import           Import


-- | A 'LibraryItem' 'Entity' & 'Book' packed into a Tuple.
type LibraryItemAndBook = (Entity LibraryItem, Book)


-- | Display all LibraryItems.
getLibraryR :: Handler Html
getLibraryR = do
    sortVal <- fromMaybe "status" <$> runInputGet (iopt textField "sort")
    items   <- runDB $ selectList [] []
    books   <- mapM (\(Entity _ i) -> runDB $ getJust $ libraryItemBook i) items
    let itemsAndBooks = sortLibrary sortVal $ zip items books
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemIsbnForm
    defaultLayout $ do
        setTitle "Library"
        $(widgetFile "library/library")


-- | Sort a Library's Items & Books.
sortLibrary :: Text -> [LibraryItemAndBook] -> [LibraryItemAndBook]
sortLibrary sortString = sortBy $ getThird sortResult
    where sortResult   = fromJust $ find ((==) sortString . getFirst)
                                    librarySortingOptions
          getFirst (a, _, _) = a
          getThird (_, _, c) = c


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
          flipComparing f a b     = comparing f b a


-- | Display a Dropdown Button to Select a Sorting Type
librarySortWidget :: Widget
librarySortWidget = $(widgetFile "library/librarySortDropdown")
