module Handler.LibraryItemEdit where

import           Data.Maybe (fromJust)
import           Import


-- | Display a Form to Edit a LibraryItem.
getLibraryItemEditR :: LibraryItemId -> Handler Html
getLibraryItemEditR libraryItemId = do
    item              <- runDB $ get404 libraryItemId
    userId            <- maybeAuthId
    when (isNothing userId) $ permissionDenied "Wrong User"
    library           <- runDB . getBy404 $ UserLibrary $ fromJust userId
    let libraryOwner  = (\(Entity _ l) -> libraryUser l) library
    when (libraryOwner /= fromJust userId) (permissionDenied "Wrong User")
    book              <- runDB . getJust $ libraryItemBook item
    (widget, enctype) <- generateFormPost $ libraryItemEditForm item
    defaultLayout $ do
        setTitle $ "Editing " `mappend` toHtml (bookTitle book)
        $(widgetFile "library/libraryEdit")

-- | Process the LibraryItem Editing Form.
postLibraryItemEditR :: LibraryItemId -> Handler Html
postLibraryItemEditR libraryItemId = do
    item              <- runDB $ get404 libraryItemId
    book              <- runDB . getJust $ libraryItemBook item
    ((result, widget), enctype) <- runFormPost $ libraryItemEditForm item
    case result of
        FormSuccess updatedItem -> runDB (replace libraryItemId $ setHasFinished updatedItem)
                                >> setMessage "Successfully updated your Library Book"
                                >> getSlug . fromJust <$> getProfile
                               >>= redirect . LibraryR
        _                       -> defaultLayout $ do
            setTitle $ "Editing " `mappend` toHtml (bookTitle book)
            $(widgetFile "library/libraryEdit")
    where setHasFinished i
            | libraryItemCompletionCount i == 0 = i { libraryItemHasFinished = False }
            | otherwise = i { libraryItemHasFinished = True }
          getSlug (_, Entity _ profile) = userProfileSlug profile
