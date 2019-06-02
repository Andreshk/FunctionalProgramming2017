-- Haskell: Data.List, pattern matching, list comprehension
-- Scheme: map, filter, foldr, accumulate
import Data.List (minimumBy, delete)
import Data.Ord (comparing)

{-
Март 2009 - идея: ако имаме функция за всички подсписъци на l2
f l1 l2 = length (filter (==l1) (sublists l2))
-}

-- Юли 2014
magic f 0 = (\x -> x)
magic f 1 = f
magic f i
  | any (\x -> f1 x /= f2 x) [0..i] = f1 . f2
  | otherwise = totalMin [ magic f x | x<-[0..i-1]]
  where f1 = magic f (i - 1)
        f2 = magic f (i - 2)
        totalMin fs = minimumBy (comparing (\f -> f 0)) fs

chainMinCompositions f = [ magic f i | i<-[0..] ]

-- Септември 2009
countMyHead lst = length (takeWhile (==(head lst)) lst)
-- За нехомогенен списък ни трябва свой тип данни
data Pair a = Only a | Both a Int
compress :: Eq a => [a] -> [Pair a]
compress [] = []
compress lst = (if count > 1 then Both el count else Only el) : compress (drop count lst)
  where el = head lst
        count = countMyHead lst

-- Септември 2014
-- пермутации на списък:
-- за всеки елемент на списъка:
-- - вземи всички пермутации на останалите
-- - и постави този елемент отпред (:)
perms [] = [[]]
perms lst = [ (x:p) | x<-lst, p<-(perms (delete x lst))]