module Handler.Library where

import           Import


-- | Display all LibraryItems.
getLibraryR :: Handler Html
getLibraryR = do
    items <- runDB $ selectList [] []
    books <- mapM (\(Entity _ i) -> runDB $ getJust $ libraryItemBook i) items
    let itemsAndBooks = zip items books
    (libraryWidget, libraryEnctype) <- generateFormPost libraryItemIsbnForm
    defaultLayout $ do
        setTitle "Library"
        $(widgetFile "library")
