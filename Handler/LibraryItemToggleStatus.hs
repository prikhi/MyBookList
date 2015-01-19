module Handler.LibraryItemToggleStatus where

import           Import


-- | Toggle the 'inProgress' attribute of a 'LibraryItem'.
getLibraryItemToggleStatusR :: LibraryItemId -> Handler Html
getLibraryItemToggleStatusR libraryItemId = do
    item <- runDB $ get404 libraryItemId
    toggleInProgressStatus $ Entity libraryItemId item
    redirect LibraryR
