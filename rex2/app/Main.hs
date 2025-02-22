{-# LANGUAGE TemplateHaskell, InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}
module Main where
import Test.QuickCheck ( quickCheckAll, Arbitrary )
import qualified Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))

-- EXERCISE 1
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
prop_map :: (Eq b, Num b) => [b] -> Bool
prop_map xs = map' (+1) xs == map (+1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x: filter' p xs
filter' p (_:xs) = filter' p xs
prop_filter :: Integral a => [a] -> Bool
prop_filter xs = filter' even xs == filter even xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)
prop_foldr :: (Eq a, Num a) => [a] -> Bool
prop_foldr xs = foldr' (+) 0 xs == foldr (+) 0 xs

-- 1.1
inc :: [Integer] -> [Integer]
inc = map' (+1)

-- 1.2
evenList :: [Integer] -> [Integer]
evenList = filter' even

-- 1.3
shortStrs :: Show a => [a] -> [String]
shortStrs xs = filter (\x-> length x <= 2) $ map show xs
prop_shortstr :: Show a => [a] -> Bool
prop_shortstr xs = all (\x-> length x <= 2) $ shortStrs xs

-- 1.4
and' :: [Bool] -> Bool
and' = foldr' (&&) True
prop_and :: [Bool] -> Bool
prop_and xs = and xs == and' xs

or' :: [Bool] -> Bool
or' = foldr' (||) False
prop_or :: [Bool] -> Bool
prop_or xs = or xs == or' xs

-- 1.5
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and' $ map p xs
prop_all :: Integral a => [a] -> Bool
prop_all xs = all' even xs == all even xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or' $ map p xs
prop_any :: Integral a => [a] -> Bool
prop_any xs =  any' even xs == any even xs

-- 1.6
length' :: [a] -> Int
length' = foldr' (\_ acc -> acc + 1) 0
prop_length :: [a] -> Bool
prop_length xs = length' xs == length xs

-- 1.7
idMap :: [a] -> [a]
idMap = map' id
prop_idmap :: Eq a => [a] -> Bool
prop_idmap xs = idMap xs == xs

idFilter :: [a] -> [a]
idFilter = filter' (const True)
prop_idfilter :: Eq a => [a] -> Bool
prop_idfilter xs = idFilter xs == xs

idFold :: [a] -> [a]
idFold = foldr' (:) []
prop_idfold :: Eq a => [a] -> Bool
prop_idfold xs = idFold xs == xs

-- 1.8
map'' :: (t -> a) -> [t] -> [a]
map'' f = foldr' (\x acc -> f x : acc) []
prop_map'' :: (Eq a, Num a) => [a] -> Bool
prop_map'' xs = map'' (+1) xs == map (+1) xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr' (\x acc -> if p x then x:acc else acc) []
prop_filter'' :: Integral a => [a] -> Bool
prop_filter'' xs = filter'' even xs == filter even xs

-- EXERCISE 2
data V3 a = V3 a a a deriving (Show, Eq)
instance (Arbitrary a) => Arbitrary (V3 a) where
  arbitrary :: Test.QuickCheck.Gen.Gen (V3 a)
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    V3 x y <$> arbitrary


mapV3 :: (a -> b) -> V3 a -> V3 b
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

liftV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
liftV3 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

negateV3 :: (Num a) => V3 a -> V3 a
negateV3 = mapV3 negate
addV3 :: (Num a) => V3 a -> V3 a -> V3 a
addV3 = liftV3 (+)
subV3 :: (Num a) => V3 a -> V3 a -> V3 a
subV3 = liftV3 (-)
mulV3 :: (Num a) => V3 a -> V3 a -> V3 a
mulV3 = liftV3 (*)
divV3 :: (Fractional a) => V3 a -> V3 a -> V3 a
divV3 = liftV3 (/)

prop_v3add :: (Eq a, Num a) => V3 a -> V3 a -> Bool
prop_v3add v1@(V3 x1 y1 z1) v2@(V3 x2 y2 z2) = 
    let (V3 x y z) = addV3 v1 v2 in x == x1+x2  && y== y1+y2 && z== z1+z2


return []
main :: IO Bool
main = do 
    $quickCheckAll