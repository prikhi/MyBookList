module Handler.LibraryItemDelete where

import           Data.Maybe (fromJust)
import           Import

getLibraryItemDeleteR :: LibraryItemId -> Handler Html
getLibraryItemDeleteR libraryItemId = do
    mUserId     <- maybeAuthId
    when (isNothing mUserId) notFound
    let userId   = fromJust mUserId
    item        <- runDB $ get404 libraryItemId
    library     <- runDB . get404 $ libraryItemLibrary item
    book        <- runDB . get404 $ libraryItemBook item
    when (userId /= libraryUser library) notFound
    runDB $ delete libraryItemId
    setMessage $ "Successfully removed " ++ toHtml (bookTitle book) ++
                 " from your Library."
    slug        <- getSlug userId
    redirect $ LibraryR slug
    where getSlug userId = do
           Entity _ profile <- runDB . getBy404 $ UniqueProfile userId
           return $ userProfileSlug profile
