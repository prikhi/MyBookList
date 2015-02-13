module Handler.LibraryItemDelete where

import           Import

getLibraryItemDeleteR :: LibraryItemId -> Handler Html
getLibraryItemDeleteR libraryItemId = do
    item <- runDB $ get404 libraryItemId
    book <- runDB . get404 $ libraryItemBook item
    runDB $ delete libraryItemId
    setMessage $ "Successfully removed " ++ toHtml (bookTitle book) ++
                 " from your Library."
    redirect LibraryR
