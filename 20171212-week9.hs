saySign :: Int -> String
saySign x = if x < 0
            then "Negative"
            else if x == 0
                 then "Zero"
                 else "Positive"

-- single-line comment
{-
multi-line
comment
-}

saySign2 :: Int -> String
saySign2 x
  | x < 0     = "Negative"
  | x == 0    = "Zero"
  | otherwise = "Positive"

saySign3 :: Int -> String
saySign3 0 = "Zero"
saySign3 x = if x > 0
             then "Positive"
             else "Negative"

x :: Int
x = 10

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Integer -> Integer
fib n = if n < 2
        then n
        else fib (n-1) + fib (n-2)

fib1 :: Integer -> Integer
fib1 n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib1 (n-1) + fib1 (n-2)

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n-1) + fib2 (n-2)

fib3 :: Integer -> Integer
fib3 n = case n of 0 -> 0
                   1 -> 1
                   n -> fib3 (n-1) + fib3 (n-2)

fibb :: Integer -> Integer
fibb n = helper 0 1 0
  where helper a b i
          | i == n    = a
          | otherwise = helper b (a+b) (i+1)

countRoots :: Int -> Int -> Int -> String
countRoots a b c
  | d < 0     = "No roots"
  | d == 0    = "One root"
  | otherwise = "Two roots"
  where d = b*b - 4*a*c
        sqrtD = sqrt (fromIntegral d)



useless 0 _ _ _ = 0
useless _ 0 _ _ = 0
useless _ _ 0 _ = 0
useless _ _ _ 0 = 0
useless a b c d = a + b + c + d

useless' a b c d
  | a == 0 || b == 0 || c == 0 || d == 0 = 0
  | otherwise                            = a+b+c+d

map _ [] = []
map f (x:xs) = f x : map f xs
