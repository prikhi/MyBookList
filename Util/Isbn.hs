{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provide utility functions related to ISBNs, such as
-- fetching metadata and converting to ISBN-13s.
module Util.Isbn where

import ClassyPrelude.Yesod
import Data.Aeson               (decode)
import Data.Maybe               (fromJust)
import qualified Data.Text as T
import Network.HTTP.Conduit     (simpleHttp)


-- | Attempt to retrieve a Book's metadata(title, author, etc.) from it's
-- ISBN.
getMetadataFromIsbn :: MonadIO m => Text -> m (Maybe ISBNdbBookMetadata)
getMetadataFromIsbn isbn = do
    let queryUrl = "http://isbndb.com/api/v2/json/PBG8N9Z5/book/" ++
                   T.unpack isbn
    webResponse  <- simpleHttp queryUrl
    return . getFirstISBNdbResult responseData $ decode webResponse

-- | Get the first item of a potential result using a selector function.
getFirstISBNdbResult :: (a -> [b]) -> Maybe a -> Maybe b
getFirstISBNdbResult listSelector mDecoded
    | isJust mDecoded && not (null . listSelector $ fromJust mDecoded) =
        fmap (unsafeHead . listSelector) mDecoded
    | otherwise = Nothing


-- | A standard response from the IsbnDB API
data ISBNdbResponse a = ISBNdbResponse { responseData :: [a] }

-- | Book Metadata from a Request to the IsbnDB API
data ISBNdbBookMetadata = ISBNdbBookMetadata
    { author :: Text -- | ^ ISBNdb's author
    , title  :: Text -- | ^ ISBNdb's title
    , isbn   :: Text -- | ^ ISBNdb's ISBN-13
    }

instance FromJSON a => FromJSON (ISBNdbResponse a) where
    parseJSON (Object o) = ISBNdbResponse <$> (o .: "data" >>= mapM parseJSON)
    parseJSON _ = error "Received invalid JSON Response from ISBNdb."

instance FromJSON ISBNdbBookMetadata where
    parseJSON (Object o) = do
        (authors :: [HashMap Text Value]) <- o .: "author_data"
        authorName <- unsafeHead authors .: "name"
        ISBNdbBookMetadata authorName <$> o .: "title" <*> o.: "isbn13"
    parseJSON _ = error "Received invalid Book Metadata from ISBNdb."
