import Data.List (minimumBy,maximumBy)
import Data.Ord (comparing)

minFst :: Ord a => [(a,b)] -> (a,b)
minFst lst = minimumBy (comparing fst) lst

-- Зад.1
type Point = (Double, Double)

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

compDist :: (Point, Point, Double) -> (Point, Point, Double) -> Ordering
compDist (_,_,d1) (_,_,d2)
    | d1 < d2   = LT
    | d1 == d2  = EQ
    | otherwise = GT
-- вариант с функцията за наредба. дефинирана по-горе
maxDistance :: [Point] -> (Point, Point)
maxDistance pts = getPts $ maximumBy compDist [(p1,p2,dist p1 p2) | p1<-pts, p2<-pts]
  where getPts (p1,p2,_) = (p1,p2)

-- може би по-неефикасен вариант, за който обаче
-- не се налага функция за наредба, ами само "критерий" за сравняване
maxDistancg :: [Point] -> (Point, Point) -- потърсете какво прави uncurry
maxDistancg pts = maximumBy (comparing $ uncurry dist) [ (p1,p2) | p1<-pts, p2<-pts ]

maxDistance3 :: (Point -> Point -> Double) -> [Point] -> (Point, Point)
maxDistance3 d pts = maximumBy (comparing $ uncurry d) [ (p1,p2) | p1<-pts, p2<-pts ]
-- сега maxDistancg = maxDistance3 dist

-- Зад.2
type Item = (String, Integer)
items :: [Item]
items = [("Milk",3), ("Bread",1), ("Yoghurt",-3),
         ("Butter",5), ("Cheese",-1), ("Pasta",2)]

soonestExpiring :: [Item] -> String
soonestExpiring its = fst $ minimumBy (comparing snd) [ i | i<-its, snd i >= 0]

numberExpiring :: [Item] -> Int
numberExpiring its = length [ i | i<-its, (snd i) < 0]

longestExpired :: [Item] -> String
longestExpired its = fst $ minimumBy (comparing snd) its

expiringItems :: [Item] -> (String, Int, String)
expiringItems its = (soonestExpiring its, numberExpiring its, longestExpired its)

-- Дърво, което съдържа произволни стойности по върховете - 
-- приема съдържания тип като параметър подобно на C++.
-- Оттам нататък винаги дървото върви с типа си - не пишем Tree,
-- както не пишем просто std::vector, ами "Tree a", "Tree Int" като
-- "std::vector<T>" и "std::vector<int>".
data Tree a = Empty | Node a (Tree a) (Tree a)

height :: Tree Int -> Int
height Empty = 0
height (Node val left right) = 1 + max (height left) (height right)

-- Зад.3
maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Node val left right) = val + max (maxSumPath left) (maxSumPath right)

-- Зад.4
prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val left right) = Node val (prune left) (prune right)

-- Зад.5
bloom :: Tree a -> Tree a
bloom Empty = Empty
bloom (Node val Empty Empty) = Node val leaf leaf
  where leaf = Node val Empty Empty
bloom (Node val left right) = Node val (bloom left) (bloom right)

-- Зад.6
data BST a = BEmpty | BNode a (BST a) (BST a)

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstinsert x BEmpty = BNode x BEmpty BEmpty
bstinsert x t@(BNode val left right)
    | x == val  = t
    | x < val   = BNode val (bstinsert x left) right
    | otherwise = BNode val left (bstinsert x right)

bstsize :: BST a -> Integer
bstsize BEmpty = 0
bstsize (BNode _ left right) = 1 + bstsize left + bstsize right

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstsearch _ BEmpty = False
bstsearch x (BNode val left right)
  | x == val  = True
  | x < val   = bstsearch x left
  | otherwise = bstsearch x right

bstFromList :: (Eq a, Ord a) => [a] -> BST a
bstFromList = foldr bstinsert BEmpty

-- забележете - тази функция няма ограничения за съдържания тип!
values :: BST a -> [a]
values BEmpty = []
values (BNode val left right) = values left ++ [val] ++ values right

bstSort :: (Eq a, Ord a) => [a] -> [a]
bstSort = values . bstFromList

-- Зад.7
-- нашият map отново ще представлява дърво, съдържащо наредени двойки
data Map k v = MEmpty | MNode (k,v) (Map k v) (Map k v)

mapinsert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v
mapinsert key val MEmpty = (MNode (key,val) MEmpty MEmpty)
mapinsert key val (MNode p@(k1,_) lt rt)
  | key == k1 = (MNode (k1,val) lt rt) -- заместваме старата с новата стойност
  | key < k1  = (MNode p (mapinsert key val lt) rt)
  | otherwise = (MNode p lt (mapinsert key val rt))

mapsearch :: (Eq k, Ord k) => k -> Map k v -> Maybe v
mapsearch _ MEmpty = Nothing
mapsearch key (MNode p@(k1,val) lt rt)
  | key == k1 = Just val
  | key < k1  = mapsearch key lt
  | otherwise = mapsearch key rt

mapFromList :: (Eq k, Ord k) => [(k,v)] -> Map k v
mapFromList = foldr (uncurry mapinsert) MEmpty

mapvalues :: Map k v -> [(k,v)]
mapvalues MEmpty = []
mapvalues (MNode p lt rt) = mapvalues lt ++ [p] ++ mapvalues rt

-- Зад.8
-- Конструкторите могат да бъдат и изградени от символи,
-- т.е. да ги извикваме и използваме инфиксно, като операторите, с които сме свикнали.
-- Има странно изискване обаче - да започват със символа ':'
data Expr = Const Double
          | Var
          | (:+:) Expr Expr
          | (:-:) Expr Expr
          | (:*:) Expr Expr
          | (:/:) Expr Expr
          | (:^:) Expr Double -- повдигане на израз на степен число
          deriving Show -- за по-лесно проследяване какви дървета се построяват

-- Тук отново използваме конструкторите за типа Expr
-- като обикновени инфиксни функции - все пак те са такива.
e1 :: Expr
e1 = (((Const 2) :*: Var) :+: (Const 1)) :^: 3

-- Можем да pattern match-ваме по тези "странни"
-- конструктори така, както pattern match-ваме обикновените конструктори.
-- А можем и да ги pattern match-ваме инфиксно :)
eval :: Expr -> Double -> Double
eval (Const c) _ = c
eval Var       x = x
eval (f :+: g) x = eval f x + eval g x
eval (f :-: g) x = eval f x - eval g x
eval (f :*: g) x = eval f x * eval g x
eval (f :/: g) x = eval f x / eval g x
eval (f :^: n) x = (eval f x)**n

-- Черешката на тортата - код, който е очевидно коректен!
-- четем (и пишем): производната на f+g е производната на f плюс производната на g
-- и т.н.
derive :: Expr -> Expr
derive (Const c) = Const 0
derive Var       = Const 1
derive (f :+: g) = (derive f) :+: (derive g) 
derive (f :-: g) = (derive f) :-: (derive g)
derive (f :*: g) = (f :*: (derive g)) :+: ((derive f) :*: g)
derive (f :/: g) = (((derive f) :*: g) :-: (f :*: (derive g))) :/: (g :^: 2)
derive (f :^: n) = ((Const n) :*: (f :^: (n-1))) :*: (derive f)
