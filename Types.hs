module Types where

import ClassyPrelude
import Database.Persist.TH  (derivePersistField)

data Priority = Lowest | Low | Medium | High | Highest
  deriving (Show, Read, Eq, Enum, Ord, Bounded)
derivePersistField "Priority"
