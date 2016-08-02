module Handler.LibraryItemAdd where

import           Data.Maybe (fromJust)

import           Import


-- | Process the addition of new Books to the Library.
postLibraryItemAddR :: Handler Html
postLibraryItemAddR = do
    mUserId         <- maybeAuthId
    when (isNothing mUserId) notFound
    library         <- runDB . getBy404 . UserLibrary $ fromJust mUserId
    let libraryId   = (\(Entity i _) -> i) library
    ((result, libraryWidget), libraryEnctype) <- runFormPost libraryItemIsbnForm
    continueEditing <- runInputPost $ iopt textField "edit"
    case result of
         FormSuccess isbn -> do
            bookId  <- fromMaybe (error "create failed") <$>
                       createBookFromIsbn isbn
            mItem   <- runDB . getBy $ UniqueLibraryBook libraryId bookId
            mItemId <- case mItem of
                 Just _   ->
                   setMessage "That Book is already in your Library" >>
                   return Nothing
                 Nothing  ->
                   setMessage "Added Book to your Library" >>
                   runDB (deleteWhere [WishlistItemBook ==. bookId]) >>
                   Just <$> createLibraryItemFromBook (fromJust mUserId) bookId
            if isJust continueEditing && isJust mItemId
                then redirect (LibraryItemEditR $ fromJust mItemId)
                else getSlug . fromJust <$> getProfile >>= redirect . LibraryR
         _                -> defaultLayout $ do
            setTitle "An Error Occured While Adding to the Library"
            $(widgetFile "library/libraryItemAddError")
    where getSlug (_, Entity _ profile) = userProfileSlug profile
