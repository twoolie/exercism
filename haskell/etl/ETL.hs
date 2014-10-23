module ETL (transform) where

import Data.Char
import Data.Map

flower = fmap toLower

transform :: Map Int [String] -> Map String Int
transform = fromList . foldlWithKey' go []
	where
		go :: [(String,Int)] -> Int -> [String] -> [(String, Int)]
		go acc score letters = acc ++ fmap (flip (,) score . flower) letters