module Types where

import           ClassyPrelude
import qualified Data.Text as T         (pack)
import           Database.Persist.TH    (derivePersistField)
import           Yesod.Core             (PathPiece(..))

data Priority = Lowest | Low | Medium | High | Highest
  deriving (Show, Read, Eq, Enum, Ord, Bounded)
derivePersistField "Priority"

instance PathPiece Priority where
    toPathPiece = T.pack . show
    fromPathPiece = readMay
