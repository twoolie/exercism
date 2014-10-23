module Atbash (encode, charmap) where

import Data.Char
import Data.List
import Data.Array

charmap :: Array Char Char
charmap = listArray ('a','z') ['z', 'y'..'a']

encode :: String -> String
encode ins = intercalate " " $ groupOf 5 $ encode' $ chars ins
  where
    chars = map toLower . filter isAlphaNum
    encode' = map (\c -> if isAlpha c then charmap ! c else c)
    groupOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)