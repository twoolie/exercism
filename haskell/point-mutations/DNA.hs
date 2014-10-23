module DNA where

hammingDistance :: String -> String -> Int
hammingDistance as bs = length $ filter not $ zipWith (==) as bs

