-- | This module contains functions & forms related to the Book model
module Model.Book where

import ClassyPrelude.Yesod
import qualified Data.Text as T
import Foundation
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..), renderBootstrap3, bfs,
                              withPlaceholder)

import Model
import Types                 (Priority)
import Util.Isbn             (getMetadataFromIsbn, ISBNdbBookMetadata(..))


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


-- | A Form for validating creation of Books from only an ISBN.
bookForm :: Form (Text, Priority)
bookForm = renderBootstrap3 BootstrapInlineForm $ (,)
    <$> areq isbnField isbnSettings Nothing
    <*> areq (selectField optionsEnum) (bfs ("Priority" :: Text)) Nothing
    where isbnField    = check validateIsbn textField
          isbnSettings = withPlaceholder "ISBN" $ bfs ("ISBN" :: Text)
          validateIsbn i
              | length i `elem` [10, 13, 14] = Right $ T.filter (/= '-') i
              | otherwise                    = Left (isbnError :: Text)
          isbnError    = "Incorrect ISBN Length"
