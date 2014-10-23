module Gigasecond (fromDay) where

import Data.Time.Calendar (Day, addDays)

fromDay :: Day -> Day
fromDay = addDays (1000000000 `div` 24*60*60)
