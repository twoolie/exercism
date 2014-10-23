module Raindrops (convert) where

import Data.List
import Control.Monad.Writer

convert :: Int -> String
convert n = execWriter $ do
    when (3 `elem` facts) $ tell "Pling"
    when (5 `elem` facts) $ tell "Plang"
    when (7 `elem` facts) $ tell "Plong"
    when (null facts)     $ tell (show n)
  where
    facts = filter (\x -> rem n x == 0) [3,5,7]