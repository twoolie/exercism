module WordCount (wordCount) where

import Data.Map (Map, fromListWith)
import Data.Char (toLower, isAlphaNum)

wordCount :: String -> Map String Int
wordCount = fromListWith (+) . map (\w -> (w,1)) . words . sanitise
  where sanitise = map $ toLower . \c -> if isAlphaNum c then c else ' '
