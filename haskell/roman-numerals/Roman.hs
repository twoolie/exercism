module Roman (numerals) where

numeralSymbols :: [(Int, String -> String)]
numeralSymbols = map (fmap (++)) [
    (1000,"M"), (900,"CM"), (500,"D"), (400,"CD"),
    (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
    (10,  "X"), (9,  "IX"), (5,  "V"), (4,  "IV"),
    (1,   "I")]

numerals :: Int -> String
numerals 0 = "N" -- N == 0
numerals n | n < 0 = '-': numerals (-n)
           | otherwise = go n numeralSymbols
  where go 0 ns = ""
        go n ns@((m,sym):rest)
           | n >= m = sym $ go (n-m) ns
           | otherwise = go n rest