{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import Import

import Data.List             (nub)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs, withPlaceholder)


getHomeR :: Handler Html
getHomeR = do
    wishlists <- runDB $ selectList [] []
    wanted    <- getMostWantedBooks
    (wishlistWidget, wishlistEnctype)         <- generateFormPost wishlistForm
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    wishlists <- runDB $ selectList [] []
    wanted    <- getMostWantedBooks
    ((result, wishlistWidget), wishlistEnctype) <- runFormPost wishlistForm
    case result of
         FormSuccess wishlist -> do _ <- runDB (insert wishlist)
                                    setMessage "Successfully added Wishlist"
                                    redirect HomeR
         _                    -> setMessage "Encountered an error while creating Wishlist"
    defaultLayout $ do
        setTitle "Welcome To MyBookList!"
        $(widgetFile "homepage")

getMostWantedBooks :: Handler [Book]
getMostWantedBooks = runDB $ do
    items     <- selectList [] []
    let books = take 5 . nub . reverse . map (\(Entity _ i) -> wishlistItemBook i)
              . sortBy (compare `on` (\(Entity _ i) -> wishlistItemPriority i))
              $ items
    mapM getJust books

wishlistForm :: Form Wishlist
wishlistForm = renderBootstrap3 BootstrapInlineForm $ Wishlist
    <$> areq textField nameSettings Nothing
    where nameSettings = withPlaceholder "Wishlist Name" $ bfs ("Name" :: Text)
