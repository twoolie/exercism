module PrimeFactors (primeFactors) where

import Data.List

-- time: 1.14s user 0.00s system 99% cpu 1.149 total

-- wheel optimisation reduces the search space for primes
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

-- lazily evaluated list of primes
primes = 2 : 3 : 5 : 7 : filter isPrime (scanl (+) 11 wheel)
  where
    isPrime n = and [ rem n p > 0 | p <- primesFBy n]
    primesFBy n = takeWhile (<(n`div`(2*3*5*7))) primes


primeFactors :: Int -> [Int]
primeFactors n = go n $ takeWhile (<=n) primes
  where
    go 1 _  = []
    go n [] = []
    go n pl@(p:ps) | rem n p == 0 = p : go (n`div`p) pl
                   | p > n        = []
                   | otherwise    = go n ps