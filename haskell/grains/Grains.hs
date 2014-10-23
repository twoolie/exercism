module Grains (square, total) where

squares = iterate (*2) 1 :: [Integer]

square n = squares !! (n-1)

total = sum $ take 64 squares
