module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal str = go str 0
	where
		go []       n = n
		go ('1':cs) n = go cs (n*2 + 1)
		go ('0':cs) n = go cs (n * 2)
		go (c:cs)   n = go cs n