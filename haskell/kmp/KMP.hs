import Data.List
import Control.Monad
import Debug.Trace
import Text.Printf

groupOf :: Int -> [a] -> [[a]]
groupOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

mkTable :: Eq a => [a] -> [[a]]
mkTable pat = snd $ mapAccumL step pat $ tail pat
  where step ps@(p:s) c
         | p == c    = (s, s)
         | otherwise = (pat, pat)

kmp_substring table text = match text (head table) table
  where
    -- match does the actual matching
    match :: String -> [String] -> Bool
    match _ [] = True
    match [] tbl = False
    match _t@(t:ts) (_m@(m:ms):mts)
      | t == m    = trace (printf "Y : %s : %s" _t _m) $ match ts mts
      | otherwise = trace (printf "N : %s : %s" _t _m) $ match ts table
    
    


main = print $ kmp_substring (mkTable "PARACHUTE") "PARTICIPATE IN PARACHUTE"

--main = do 
--  _ <- getLine
--  ls <- fmap lines getContents
--  forM_ (groupOf 2 ls) $ \[text, mat] ->
--    putStrLn $ if kmp_substring text mat then "YES" else "NO"

