module Handler.LibraryItemAdd where

import           Import
import           Data.Maybe (fromJust)


-- | Process the addition of new BOoks to the Library.
postLibraryItemAddR :: Handler Html
postLibraryItemAddR = do
    ((result, libraryWidget), libraryEnctype) <- runFormPost libraryItemIsbnForm
    continueEditing <- runInputPost $ iopt textField "edit"
    case result of
         FormSuccess isbn -> do
            bookId  <- fromMaybe (error "create failed") <$>
                       createBookFromIsbn isbn
            mItem   <- runDB . getBy $ UniqueLibraryBook bookId
            mItemId <- case mItem of
                 Just _   -> setMessage "That Book is already in your Library"
                          >> return Nothing
                 Nothing  -> setMessage "Added Book to your Library" >>
                             Just <$> createLibraryItemFromBook bookId
            if isJust continueEditing && isJust mItemId
                then redirect (LibraryItemEditR $ fromJust mItemId)
                else redirect LibraryR
         _                -> defaultLayout $ do
            setTitle "An Error Occured While Adding to the Library"
            $(widgetFile "libraryItemAddError")
