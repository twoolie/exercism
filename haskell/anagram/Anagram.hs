module Anagram where

import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor word list = filter (isAnagramOf word) list

isAnagramOf word candidate = not sameWord && sameLetters
  where
  	(lword, lcand)          = (map toLower word, map toLower candidate)
  	(sameLetters, sameWord) = (sort lword == sort lcand, lword == lcand)