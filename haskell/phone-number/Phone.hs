module Phone (areaCode, number, prettyPrint) where

import Data.Char
import Text.Printf

invalid = "0000000000"

strip = filter isDigit

number :: String -> String 
number raw = validate $ strip raw
  where validate nums@(n:ns)
          | length nums == 10 = nums
          | length nums == 11 && n == '1' = ns
          | otherwise = invalid
        validate _ = invalid

areaCode :: String -> String
areaCode = take 3 . number


prettyPrint :: String -> String
prettyPrint num = printf "(%s) %s-%s" area prefix suffix
  where clean = number num
        area = take 3 clean
        prefix = take 3 $ drop 3 clean
        suffix = drop 6 clean