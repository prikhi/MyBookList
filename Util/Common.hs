-- | This module contains utility functions available to all Handlers.
module Util.Common where

import           ClassyPrelude


-- | Compare using a selector function, reversing the 'Ordering'.
flipComparing :: Ord a => (b -> a) -> b -> b -> Ordering
flipComparing f a b = comparing f b a
