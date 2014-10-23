module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year = divis 4 && not (divis 100) || (divis 400)
  where divis n = year `rem` n == 0