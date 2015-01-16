-- | The module contains utility functions commonly used in Templates.
module Util.Templates where

import ClassyPrelude
import qualified Data.Text as T


-- | Return a singlular or pluralized string depending on the value of n.
pluralize :: (Num a, Eq a)
          => a      -- ^ The item count.
          -> Text   -- ^ The singular string.
          -> Text   -- ^ The plural string.
          -> Text
pluralize n sing plur
    | n == 1    = sing
    | otherwise = plur

-- | Format a 'Day' into a string like `Apr 20th, 2015`.
formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%b %e, %Y"
