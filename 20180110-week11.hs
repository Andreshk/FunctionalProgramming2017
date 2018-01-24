import Data.List (sort,nub,sortBy)
import Data.Ord (comparing)

-- Зад.1
sumProducts :: Num a => [[a]] -> a
--sumProducts ll = sum (map product ll)
--sumProducts ll = sum $ map product ll
--sumProducts ll = sum [ product lst | lst<-ll ]
sumProducts = sum . (map product)

sumProducts' ll = helper 0 1 ll
  where helper tmpS tmpP ll
          | null ll      = tmpS
          | null (head ll) = helper (tmpS+tmpP) 1 (tail ll)
          | otherwise      = helper tmpS (tmpP*head h) ((tail h):t)
          where (h:t) = ll

-- Зад.2
occurences :: (Eq a) => [a] -> [a] -> [Int]
occurences l1 l2 = [ count x l2 | x<-l1 ]
  where count :: (Eq a) => a -> [a] -> Int
        count x = length . filter (==x)

-- Зад.3
mainDiag :: [[a]] -> [a]
mainDiag [] = []
mainDiag ((x:_):rows) = x : mainDiag (map tail rows)

mainDiag' :: [[a]] -> [a]
mainDiag' m = [ (m!!idx)!!idx | idx<-[0..l] ]
  where l = (length m) - 1
--              m[idx][idx]

-- Зад.4
isSquare :: [[a]] -> Bool
--isSquare m = all (\row -> length row == numRows) m
--  where numRows = length m
--isSquare m = and [ length row == numRows | row<-m, let numRows = length m ]
isSquare m = all ((==numRows) . length) m
  where numRows = length m

-- Зад.5
sndDiag :: [[a]] -> [a]
sndDiag = mainDiag . map reverse

-- Зад.6 - на практика почти като зад.4
matchLengths :: [[a]] -> Bool
matchLengths [] = True
matchLengths lsts = all ((==l) . length) lsts
  where l = length $ head lsts

-- Зад.7
-- Един подход при тези задачи е да "обхождаме" списъците подобно на merge
setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion s1 [] = s1
setUnion [] s2 = s2
setUnion (x:xs) (y:ys)
  | x < y  = x : setUnion xs (y:ys)
  | x == y = x : setUnion xs ys
  | x > y  = y : setUnion (x:xs) ys

setIntersect :: (Eq a, Ord a) => [a] -> [a] -> [a]
setIntersect s1 [] = []
setIntersect [] s2 = []
setIntersect (x:xs) (y:ys)
  | x < y  = setIntersect xs (y:ys)
  | x == y = x : setIntersect xs ys
  | x > y  = setIntersect (x:xs) ys

setDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
setDiff s1 [] = s1
setDiff [] s2 = []
setDiff (x:xs) (y:ys)
  | x < y  = x : setDiff xs (y:ys)
  | x == y = setDiff xs ys
  | x > y  = setDiff (x:xs) ys

setSymDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
setSymDiff s1 s2 = setUnion (setDiff s1 s2) (setDiff s2 s1)

-- а за някои функции можем просто да използваме filter или list comprehension
setUnion2 s1 s2 = sort $ nub $ s1 ++ s2
setIntersect2 s1 s2 = filter (`elem` s2) s1
setDiff2 s1 s2 = filter (not . (`elem` s2)) s1

setIntersect3 s1 s2 = [ x | x<-s1, x `elem` s2]
setDiff3 s1 s2 = [ x | x<-s1, not x `elem` s2]

-- Зад.9
specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort lsts = map fst $ sortBy (comparing snd) [ (x, mostFreq x) | x<-lsts ]
  where mostFreq lst = fst . head . sortBy compPairs $ [ (el, count el lst) | el<-lst ]
        compPairs (x,c1) (y,c2) -- специална функция за наредба на наредените двойки
          | c1 /= c2  = compare c1 c2
          | otherwise = compare x y
        count el lst = length $ filter (==el) lst
