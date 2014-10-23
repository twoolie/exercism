module Beer where

import Data.Char

title (c:cs) = toTitle c : cs

sing from to = unlines $ map verse $ reverse [to..from]

verse n = unlines [ title $ nbottles n ++ " of beer on the wall, " ++ nbottles n ++ " of beer."
                  , case n of 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall."
                              _ -> "Take " ++ pronoun n ++ " down and pass it around, "
                                           ++ nbottles (n-1) ++ " of beer on the wall." ]
  where nbottles 0 = "no more bottles"
        nbottles 1 = "1 bottle"
        nbottles n = show n ++ " bottles"
        pronoun 1 = "it"
        pronoun _ = "one"
