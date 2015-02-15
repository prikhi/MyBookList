-- | This module contains functions for generating & manipulating Widgets.
module Util.Widgets where

import           Data.Maybe   (fromJust)

import           Import

-- | The GET value for a Sorting Option.
type SortValue     = Text
-- | The display name for a Sorting Option.
type SortName      = Text
-- | A list of possible Sorting Options for a data type.
type SortOptions a = [(SortValue, SortName, a -> a -> Ordering)]


-- | Create a dropdown widget for selecting Sorting Options.
sortWidget :: SortOptions a -> Widget
sortWidget sortOptions  =
    let dropdownOptions = map (\(a, b, _) -> (a, b)) sortOptions
     in $(widgetFile "widgets/sortDropdownWidget")


-- | Sort the list using the comparison function of the given 'SortValue'.
sortListByOption :: SortOptions a -> SortValue -> [a] -> [a]
sortListByOption sortOptions sortValue = sortBy $ getThird sortResult
    where getFirst (a, _, _) = a
          getThird (_, _, c) = c
          sortResult         = fromJust $ find ((==) sortValue . getFirst)
                               sortOptions

-- | Attempt to pull a 'SortValue' from the GET Parameters, falling back to
-- a specified default.
getSortValue  :: SortValue -> Handler SortValue
getSortValue val = fromMaybe val <$> runInputGet (iopt textField "sort")
