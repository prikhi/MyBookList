-- | This module contains custom Fields used in the application.
module Util.Fields where

import ClassyPrelude.Yesod
import qualified Data.Text as T


-- | A field validating for ISBN length, with or without the dash in the
-- ISBN-13.
isbnField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
          => Field m Text
isbnField              = check validateIsbn textField
    where isbnError    = "Incorrect ISBN Length"
          validateIsbn i
              | length i `elem` [10, 13, 14] = Right $ T.filter (/= '-') i
              | otherwise                    = Left (isbnError :: Text)
