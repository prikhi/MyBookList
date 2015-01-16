module Handler.LibraryItemAdd where

import Import


-- | Process the addition of new BOoks to the Library.
postLibraryItemAddR :: Handler Html
postLibraryItemAddR = do
    ((result, libraryWidget), libraryEnctype) <- runFormPost libraryItemForm
    case result of
         FormSuccess isbn -> do
            bookId <- fromMaybe (error "create failed") <$>
                      createBookFromIsbn isbn
            mItem  <- runDB . getBy $ UniqueLibraryBook bookId
            _      <- case mItem of
                 Just _   -> setMessage "That Book is already in your Library"
                 Nothing  -> createLibraryItemFromBook bookId >>
                             setMessage "Added Book to your Library"
            redirect $ HomeR
         _                -> defaultLayout $ do
            setTitle $ "An Error Occured While Adding to the Library"
            $(widgetFile "libraryItemAddError")
