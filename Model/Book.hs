-- | This module contains functions & forms related to the Book model
module Model.Book where

import           ClassyPrelude.Yesod
import           Foundation

import           Model
import           Util.Isbn           (ISBNdbBookMetadata (..),
                                      getMetadataFromIsbn)


-- | Create a new Book from an ISBN, pulling information from an ISBN
-- database. Returns Nothing on failure.
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
