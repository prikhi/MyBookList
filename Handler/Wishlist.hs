{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Wishlist where

import Import

import Data.Aeson               (decode)
import Data.Maybe               (fromJust)
import qualified Data.Text as T
import Network.HTTP.Conduit     (simpleHttp)
import Text.Julius              (rawJS)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..), renderBootstrap3, bfs,
                              withPlaceholder)

import Types                    (Priority(Medium))

getWishlistR :: Text -> Handler Html
getWishlistR name = do
    Entity wishlistId wishlist  <- runDB . getBy404 $ UniqueWishlistName name
    books                       <- getBooksInWishlist wishlistId
    (widget, enctype)           <- generateFormPost bookForm
    defaultLayout $ do
        setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
        $(widgetFile "wishlist")

postWishlistR :: Text -> Handler Html
postWishlistR name = do
    Entity wishlistId wishlist  <- runDB . getBy404 $ UniqueWishlistName name
    books                       <- getBooksInWishlist wishlistId
    ((result, widget), enctype) <- runFormPost bookForm
    case result of
        FormSuccess formInstance -> do
            bookid <- fromMaybe (error "create failed") <$>
                      createBookFromIsbn (fst formInstance)
            mItem  <- runDB $ getBy $ WishlistBook wishlistId bookid
            case mItem of
                 Just _  -> setMessage "That Book is already in your Wishlist"
                 Nothing -> runDB (insert $ WishlistItem bookid wishlistId
                                            $ snd formInstance)
                         >> setMessage "Added Book to your Wishlist"
            redirect $ WishlistR name
        _                -> defaultLayout $ do
            setTitle $ toHtml $ wishlistName wishlist `mappend` " Wishlist"
            $(widgetFile "wishlist")

createBookFromIsbn :: Text -> Handler (Maybe BookId)
createBookFromIsbn i = do
    mMetadata <- getMetadataFromIsbn i
    case mMetadata of
        Just bookMeta -> do
            mBook  <- runDB $ getBy . UniqueIsbn $ isbn bookMeta
            case mBook of
                    Just (Entity b _)  -> return $ Just b
                    Nothing -> do x <- runDB $ insert $ Book (isbn bookMeta)
                                        (title bookMeta) (author bookMeta)
                                  return $ Just x
        Nothing       -> return Nothing

bookForm :: Html -> MForm Handler (FormResult (Text, Priority), Widget)
bookForm = renderBootstrap3 BootstrapInlineForm $ (,)
    <$> areq isbnField isbnSettings Nothing
    <*> areq (selectField optionsEnum) (bfs ("Priority" :: Text)) Nothing
    where isbnField    = check validateIsbn textField
          isbnSettings = withPlaceholder "ISBN" $ bfs ("ISBN" :: Text)
          validateIsbn i
              | length i `elem` [10, 13, 14] = Right $ T.filter (/= '-') i
              | otherwise                    = Left (isbnError :: Text)
          isbnError    = "Incorrect ISBN Length"

getMetadataFromIsbn :: MonadIO m => Text -> m (Maybe XIsbnBookMetadata)
getMetadataFromIsbn isbn = do
    let queryUrl = "http://isbndb.com/api/v2/json/PBG8N9Z5/book/" ++
                   T.unpack isbn
    webResponse  <- simpleHttp queryUrl
    return . getFirstXIsbnResult list $ decode webResponse

getFirstXIsbnResult :: (a -> [b]) -> Maybe a -> Maybe b
getFirstXIsbnResult listSelector mDecoded
    | isJust mDecoded && not (null . listSelector $ fromJust mDecoded) =
        fmap (unsafeHead . listSelector) mDecoded
    | otherwise = Nothing

-- xisbn.worldcat.org Data Types
data XIsbnResponse a = XIsbnResponse { list :: [a] }

data XIsbnTo13 = XIsbnTo13
    { isbn13 :: [Text] }

data XIsbnBookMetadata = XIsbnBookMetadata
    { author :: Text , title :: Text , isbn :: Text }

instance FromJSON a => FromJSON (XIsbnResponse a) where
    parseJSON (Object o) = XIsbnResponse <$> (o .: "data" >>= mapM parseJSON)
    parseJSON _ = error "Received invalid Metadata Response from xisbn."

instance FromJSON XIsbnBookMetadata where
    parseJSON (Object o) = do
        (authors :: [HashMap Text Value]) <- o .: "author_data"
        authorName <- unsafeHead authors .: "name"
        XIsbnBookMetadata authorName <$> o .: "title" <*> o.: "isbn13"
    parseJSON _ = error "Received invalid Book Metadata from xisbn."

instance FromJSON XIsbnTo13 where
    parseJSON (Object o) = XIsbnTo13 <$> (o .: "isbn" >>= mapM parseJSON)
    parseJSON _ = error "Received invalid ISBN-13 from xisbn."
