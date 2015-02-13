module Handler.LibraryItemEdit where

import           Import


-- | Display a Form to Edit a LibraryItem.
getLibraryItemEditR :: LibraryItemId -> Handler Html
getLibraryItemEditR libraryItemId = do
    item              <- runDB $ get404 libraryItemId
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
                                >> redirect LibraryR
        _                       -> defaultLayout $ do
            setTitle $ "Editing " `mappend` toHtml (bookTitle book)
            $(widgetFile "library/libraryEdit")
    where setHasFinished i
            | libraryItemCompletionCount i == 0 = i { libraryItemHasFinished = False }
            | otherwise = i { libraryItemHasFinished = True }
