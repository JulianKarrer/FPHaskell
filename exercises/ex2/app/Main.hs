module Main where

-- EXERCISE 1
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : (map' f xs)

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ val [] = val
foldr' f val (x:xs) = foldr' f (f x val) xs

-- 1.1
inc :: [Integer] -> [Integer]
inc = map' (+1)

-- 1.2
evenList :: [Integer] -> [Integer]
evenList = filter' even

-- 1.3
shortStrs :: [Integer] -> [String]
shortStrs xs = filter (\x -> length x <= 2) (map' show xs)

-- 1.4
and' :: [Bool] -> Bool
and' = foldr' (&&) True

or' :: [Bool] -> Bool
or' = foldr' (||) False

-- 1.5
all' :: [a] -> (a -> Bool) -> Bool
all' xs p = and' (map' p xs)

any' :: [a] -> (a -> Bool) -> Bool
any' xs p = or' (map p xs)

-- 1.6
length' :: [a] -> Integer
length' = foldr' (const (+ 1)) 0

-- 1.7
idMap :: [a] -> [a]
idMap = map' id

idFilter :: [a] -> [a]
idFilter = filter' (const True)

idFold :: [a] -> [a]
idFold = foldr' (\x ys -> ys ++ [x]) []

-- 1.8
map'' :: (a->b) -> [a] -> [b]
map'' f = foldr' (\x ys -> f x : ys) []

filter'' :: (a->Bool) -> [a] -> [a]
-- filter'' p = foldr' (\x ys -> ys ++ ([x | p x])) []
filter'' p = foldr' (\x ys -> if p x then x:ys else ys) []
-- foldR lambda: the right argument has already been folded, left is the new element

-- EXERCISE 2
data V3 a = V3 a a a deriving (Show, Eq)

mapV3 :: (a -> b) -> V3 a -> V3 b 
-- function arrow associates to right, equivalent to (a -> b) -> (V3 a -> V3 b)
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

liftV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
liftV3 op (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (op x1 x2) (op y1 y2) (op z1 z2)

-- implement unary operators
negV3 :: Num a => V3 a -> V3 a
negV3 = mapV3 (\x -> -x)
-- implement bindary operators
addV3 :: Num a => V3 a -> V3 a -> V3 a
addV3 = liftV3 (+)
subV3 :: Num a => V3 a -> V3 a -> V3 a
subV3 = liftV3 (-)
mulV3 :: Num a => V3 a -> V3 a -> V3 a
mulV3 = liftV3 (*)
divV3 :: Integral a => V3 a -> V3 a -> V3 a
divV3 = liftV3 div

main :: IO ()
main = putStrLn "Hello, Haskell!"
