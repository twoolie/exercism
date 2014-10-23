module DNA (toRNA) where

toRNA :: String -> String
toRNA = map (\c -> if c == 'T' then 'U' else c)