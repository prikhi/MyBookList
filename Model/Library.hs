-- | This module contains forms & functions associated with the Library
-- models.
module Model.Library where

import           ClassyPrelude.Yesod
import qualified Data.Text             as T
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3, withPlaceholder)

import           Foundation
import           Model
import           Util.Fields           (bfsText, isbnField)


-- | Return a zipped list of LibraryItems & Books from a selectList query.
getLibraryItemsAndBooks :: [Filter LibraryItem] -> [SelectOpt LibraryItem]
                        -> Handler [(Entity LibraryItem, Book)]
getLibraryItemsAndBooks filters options = do
    items <- runDB $ selectList filters options
    books <- mapM (runDB . getJust . libraryItemBook . entityItem) items
    return $ zip items books
    where entityItem (Entity _ i) = i


-- | Retrieve 5 Books currently being read.
getInProgressBooks :: Handler [(Entity LibraryItem, Book)]
getInProgressBooks = getLibraryItemsAndBooks [LibraryItemInProgress ==. True]
                                             [LimitTo 5]

-- | Retrieve the 5 most recently finished Books.
getRecentlyFinishedBooks :: Handler [(Entity LibraryItem, Book)]
getRecentlyFinishedBooks = getLibraryItemsAndBooks
                                [ LibraryItemLastFinishedOn !=. Nothing ]
                                [ Desc LibraryItemLastFinishedOn
                                , LimitTo 5 ]

-- | Retrieve the 5 Books most recently added to the Library.
getNewlyAddedBooks :: Handler [(Entity LibraryItem, Book)]
getNewlyAddedBooks = getLibraryItemsAndBooks [] [ Desc LibraryItemAddedOn
                                                , LimitTo 5 ]

-- | Display nicely formtted text for number of times finished. This uses
-- Once and Twice, then switches to numbers.
getFinishedText :: LibraryItem -> Text
getFinishedText item = if libraryItemHasFinished item
    then "Finished " `mappend` timesText (libraryItemCompletionCount item)
    else "Unread"
    where timesText 1 = "Once"
          timesText 2 = "Twice"
          timesText n = T.pack (show n) `mappend` " Times"


-- | Create a LibraryItem from a Book. Assumes unrated, not reading and
-- never completed.
createLibraryItemFromBook :: BookId -> Handler LibraryItemId
createLibraryItemFromBook bookId = do
    creationDay <- utctDay <$> liftIO getCurrentTime
    runDB . insert $ LibraryItem
        { libraryItemBook            = bookId
        , libraryItemRating          = Nothing
        , libraryItemInProgress      = False
        , libraryItemHasFinished     = False
        , libraryItemCompletionCount = 0
        , libraryItemAddedOn         = creationDay
        , libraryItemFirstFinishedOn = Nothing
        , libraryItemLastFinishedOn  = Nothing
        }

-- | Toggle the inProgress status of a 'LibraryItem'. If finishing a book,
-- the last (& possibly first) finished date is set to the current day, and
-- the completionCount is increased.
toggleInProgressStatus :: Entity LibraryItem -> Handler ()
toggleInProgressStatus (Entity itemId item)
    | libraryItemInProgress item = do
        currentDay <- liftIO $ utctDay <$> getCurrentTime
        let updates = [ LibraryItemHasFinished      =. True
                      , LibraryItemCompletionCount +=. 1
                      , LibraryItemInProgress       =. False
                      , LibraryItemLastFinishedOn   =. Just currentDay ] ++
                      [ LibraryItemFirstFinishedOn  =. Just currentDay
                            | isNothing $ libraryItemFirstFinishedOn item ]
        runDB $ update itemId updates
    | otherwise                  = runDB $ update itemId
                                         [ LibraryItemInProgress =. True ]


-- | A Form for creation of LibraryItems from only an ISBN.
libraryItemIsbnForm :: Form Text
libraryItemIsbnForm        = renderBootstrap3 BootstrapInlineForm $ id
    <$> areq isbnField isbnSettings Nothing
    where isbnSettings = withPlaceholder "ISBN" $ bfsText "ISBN"

-- | A Form for full editing of LibraryItems.
libraryItemEditForm :: LibraryItem -> Form LibraryItem
libraryItemEditForm item = renderBootstrap3 BootstrapBasicForm $
    LibraryItem bookId
    <$> aopt doubleField (bfsText  "Rating") (Just $ libraryItemRating item)
    <*> areq checkBoxField (bfsText "Currently Reading?") (Just $ libraryItemInProgress item)
    <*> areq checkBoxField (bfsText "Finished Already?") (Just $ libraryItemHasFinished item)
    <*> areq intField (bfsText "Times Finished") (Just $ libraryItemCompletionCount item)
    <*> areq dayField (bfsText "Added On") (Just $ libraryItemAddedOn item)
    <*> aopt dayField (bfsText "First Finished") (Just $ libraryItemFirstFinishedOn item)
    <*> aopt dayField (bfsText "Last Finished") (Just $ libraryItemLastFinishedOn item)
  where bookId = libraryItemBook item
