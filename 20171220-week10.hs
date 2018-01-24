-- Зад.1
gcd' :: Int -> Int -> Int
gcd' 0 b = b
gcd' a 0 = a
gcd' a b
  | a > b     = gcd' b (a `mod` b)
  | otherwise = gcd' a (b `mod` a)

-- тук знаем, че делението винаги ще е точно, но
-- не можем да използваме (/)
lcm' :: Int -> Int -> Int
lcm' a b = (a*b) `div` (gcd' a b)

-- Зад.2
ackermann :: Int -> Int -> Int
ackermann m n
  | m == 0    = n + 1
  | n == 0    = ackermann (m-1) 1
  | otherwise = ackermann (m-1) (ackermann m (n-1))

-- Зад.3
-- modulus p = sqrt (a^2 + b^2)
--   where a = fst p
--         b = snd p

modulus :: (Floating a) => (a,a) -> a
--modulus (3,4) = 5 -- за демонстрация, че можем да pattern-match-ваме
--modulus (0,y) = y -- и стойности в наредени двойки
--modulus (x,0) = x
modulus (x,y) = sqrt (x^2 + y^2)

modulus3 (x,y,z) = sqrt (x^2 + y^2 + z^2)

-- Зад.4
complAdd :: Num a => (a,a) -> (a,a) -> (a,a)
complAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

complSub :: Num a => (a,a) -> (a,a) -> (a,a)
complSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

complMul :: Num a => (a,a) -> (a,a) -> (a,a)
complMul (x1,y1) (x2,y2) = (x1*x2-y1*y2, x1*y2+x2*y1)

-- Зад.5
distance :: Floating a => (a,a) -> (a,a) -> a
distance p1 p2 = modulus (complSub p1 p2)

-- Зад.6
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- Зад.7
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
--take' n lst = (head lst) : take' (n-1) (tail lst)
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 lst = lst
drop' n (_:xs) = drop' (n-1) xs

-- Зад.8
map' f lst = [f x | x<-lst ]
filter' p lst = [ x | x<-lst, p x ]
divisors n = length [ d | d<-[1..n], n `mod` d == 0 ]
fact n = product [1..n]

prime :: Integral a => a -> Bool 
prime 1 = False
prime n = null [ x | x<-[2..(n-1)], n `mod` x == 0]

descartes :: [a] -> [b] -> [(a,b)]
descartes lst1 lst2 = [ (x,y) | x<-lst1, y<-lst2 ]

-- Зад.9
primes :: Integral a => [a]
primes = filter prime [1..]

-- Зад.10
primes' :: Integral a => [a]
primes' = sieve [2..]
  where sieve (x:xs) = x : (sieve $ filter (\y -> y`mod`x/=0) xs)

-- Бонус:
quicksort :: (Eq a, Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort lst = quicksort (filter (<pivot) lst)
             ++           (filter (==pivot) lst)
             ++ quicksort (filter (>pivot) lst)
  where pivot = head lst

-- Зад.11
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- Зад.12
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- Зад.13
compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress lst = (h, cnt) : compress rest
  where h = head lst
        cnt = length $ takeWhile (== h) lst
        rest = drop cnt lst

-- Зад.14
--maxRepeated lst = maximum $ map snd $ compress lst
--maxRepeated = maximum . (map snd) . compress
maxRepeated' lst = maximum [ y | (_,y) <- compress lst ]

-- Зад.15
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) = x : makeSet (filter (/=x) xs)

-- Зад.16
histogram :: Eq a => [a] -> [(a,Int)]
histogram lst = [ (x,count x lst) | x<-lst ]
  where count x lst = length $ filter (==x) lst

-- Зад.17 е в друго упражнение
-- Зад.18
compositions :: (a -> a) -> [(a -> a)]
compositions f = f : map (f.) (compositions f)
