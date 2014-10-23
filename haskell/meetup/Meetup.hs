module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Schedule = First | Second | Third | Fourth | Last | Teenth deriving (Show)
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq ,Enum)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay sched day year month = pickSchedule sched $ matchDay days
  where daysInMonth = [1 .. gregorianMonthLength year month]
        days = map (fromGregorian year month) daysInMonth
        matchDay = filter (matchesWeekday day)

matchesWeekday :: Weekday -> Day -> Bool
matchesWeekday day date = [Monday .. Sunday] !! (dayOfWeek-1) == day
  where (_,_,dayOfWeek) = toWeekDate date

pickSchedule :: Schedule -> [Day] -> Day
pickSchedule First  = (!! 0)
pickSchedule Second = (!! 1)
pickSchedule Third  = (!! 2)
pickSchedule Fourth = (!! 3)
pickSchedule Last   = last
pickSchedule Teenth = head . filter (isTeen . toGregorian)
  where isTeen (_,_,nday) = nday>12 && nday<20