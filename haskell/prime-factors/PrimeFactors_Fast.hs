module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = go possiblePrimes
 where
  go _ 1 = []
  go (x:xs) n
    | x `divides` n = x : go (x:xs) (n `div` x)
    | otherwise     = go xs n

  divides x y = y `rem` x == 0
  possiblePrimes = 2 : 3 : [f n | n <- [6, 12..], f <- [pred, succ]]