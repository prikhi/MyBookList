-- | This module contains functions & forms related to the Book model
module Model.Book where

import           ClassyPrelude.Yesod
import qualified Data.Text           as T (filter)

import           Foundation
import           Model
import           Util.Isbn           (ISBNdbBookMetadata (..),
                                      getMetadataFromIsbn)


-- | Create a new Book from an ISBN, pulling information from an ISBN
-- database. Returns Nothing on failure.
createBookFromIsbn :: Text -> Handler (Maybe BookId)
createBookFromIsbn i = do
    let cleanIsbn    = T.filter isAlphaNum i
    mExistingBook   <- runDB . getBy $ UniqueIsbn cleanIsbn
    case mExistingBook of
        Just (Entity b _) -> return $ Just b
        Nothing           -> getMetadataFromIsbn cleanIsbn >>=
                             maybe (return Nothing) createIfNoIsbn
    where isAlphaNum = flip elem $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
          createIfNoIsbn metadata = do
            mBook   <- runDB $ getBy . UniqueIsbn $ isbn metadata
            case mBook of
                    Just (Entity b _)  -> return $ Just b
                    Nothing            -> Just <$> runDB (insert $
                        Book (isbn metadata) (title metadata) (author metadata))
