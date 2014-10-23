module DNA where

import Data.Maybe
import Data.Map as M hiding (map)

emptyNucleotideList :: [(Char, Int)]
emptyNucleotideList = [('A', 0), ('T', 0), ('C', 0), ('G', 0)]

nucleotideCounts :: String -> Map Char Int
nucleotideCounts dna = fromListWith (+) $ emptyNucleotideList ++ map (flip (,) 1) dna

count :: Char -> String -> Int
count c dna 
  | not $ c `elem` "ATCGU" = error $ "invalid nucleotide '" ++ [c] ++ "'"
  | otherwise = maybe 0 id $ M.lookup c $ nucleotideCounts dna
