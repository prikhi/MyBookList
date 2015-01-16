-- | This module contains forms & functions associated with the Library
-- models.
module Model.Library where

import ClassyPrelude.Yesod
import Yesod.Form.Bootstrap3    (BootstrapFormLayout(..), renderBootstrap3,
                                 bfs, withPlaceholder)

import Foundation
import Model
import Util.Fields              (isbnField)


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


-- | A Form for creation of LibraryItems from only an ISBN.
libraryItemForm :: Form Text
libraryItemForm        = renderBootstrap3 BootstrapInlineForm $ id
    <$> areq isbnField isbnSettings Nothing
    where isbnSettings = withPlaceholder "ISBN" $ bfs ("ISBN" :: Text)
