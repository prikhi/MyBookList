module Handler.LibraryItemToggleStatus where

import           Data.Maybe (fromJust)

import           Import


-- | Toggle the 'inProgress' attribute of a 'LibraryItem'.
getLibraryItemToggleStatusR :: LibraryItemId -> Handler Html
getLibraryItemToggleStatusR libraryItemId = do
    userId            <- maybeAuthId
    when (isNothing userId) $ permissionDenied "Wrong User"
    userSlug          <- getSlug . fromJust <$> getProfile
    library           <- runDB . getBy404 $ UserLibrary $ fromJust userId
    let libraryOwner  = (\(Entity _ l) -> libraryUser l) library
    when (libraryOwner /= fromJust userId) (permissionDenied "Wrong User")
    item <- runDB $ get404 libraryItemId
    toggleInProgressStatus $ Entity libraryItemId item
    redirect $ LibraryR userSlug
    where getSlug (_, Entity _ profile) = userProfileSlug profile
