module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
    | a+b<=c || b+c<=a || c+a<=b = Illogical
    | s1 && s2 && s3 = Equilateral
    | s1 || s2 || s3 = Isosceles
    | otherwise = Scalene
  where (s1,s2,s3) = (a==b,b==c,c==a)