{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use sum" #-}
module Main where
import Test.QuickCheck ( quickCheckAll )
import Data.List (sort, intersperse)
-- EXERCISE 1
-- 1.1
maxi :: Integer -> Integer -> Integer
maxi a b | a >= b = a
maxi _ b          = b
prop_maxi :: Integer -> Integer -> Bool
prop_maxi a b = max a b == maxi a b

mini :: Integer -> Integer -> Integer
mini a b | a <= b = a
mini _ b          = b
prop_mini :: Integer -> Integer -> Bool
prop_mini a b = min a b == mini a b

-- 1.2
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = a `maxi` b `maxi` c
prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 a b c = max3 a b c == max a (max b c)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (a, b, c) = a `maxi` b `maxi` c
prop_max3Tupled :: (Integer, Integer, Integer) -> Bool
prop_max3Tupled (a,b,c) = max3Tupled (a,b,c) == max a (max b c)

-- 1.3
med :: Integer -> Integer -> Integer -> Integer
med a b c = max3 (mini a b) (mini a c) (mini b c)
prop_med :: Integer -> Integer -> Integer -> Bool
prop_med a b c = med a b c == sort [a,b,c] !! 1

-- EXERCISE 2
-- 2.1
null' :: [a] -> Bool
null' []    = True
null' (_:_) = False
prop_null :: [a] -> Bool
prop_null xs = null' xs == null xs

-- 2.2
sum' :: Num a => [a] -> a
sum' = foldr (+) 0
prop_sum :: (Eq a, Num a) => [a] -> Bool
prop_sum xs = sum' xs == sum xs

-- 2.3
concat' :: [[a]] -> [a]
concat' = foldr (++) []
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xs = concat' xs == concat xs

-- 2.4
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:_) | x==y = True
elem' x (_:ys)       = elem' x ys
prop_elem :: Eq a => a -> [a] -> Bool
prop_elem x ys = elem' x ys == elem x ys

-- 2.5
take' :: Int -> [a] -> [a]
take' n (x : xs) | n>0 = x : take' (n-1) xs
take' _ _ = []
prop_take :: Eq a => Int -> [a] -> Bool
prop_take n xs = take' n xs == take n xs

-- 2.6
drop' :: Int -> [a] -> [a]
drop' n (_:xs) | n>0 = drop' (n-1) xs
drop' _ xs  = xs
prop_drop :: Eq a => Int -> [a] -> Bool
prop_drop n xs = drop' n xs == drop n xs

-- 2.7
last' :: [a] -> a
last' [] = error "last' called on an empty list"
last' [x] = x
last' (_:xs) = last' xs
prop_last :: Eq a => [a] -> Bool
prop_last xs = null xs || (last' xs == last xs)

lastSafe :: [a] -> Maybe a
lastSafe [] = Nothing
lastSafe [x] = Just x
lastSafe (_:xs) = lastSafe xs
prop_lastsafe :: Eq a => [a] -> Bool
prop_lastsafe xs = lastSafe xs == if null xs then Nothing else Just (last xs)

-- 2.8
init' :: [a] -> [a]
init' [] = error "init' called on an empty list"
init' [_] = []
init' (x:xs) = x : init' xs
prop_init :: Eq a => [a] -> Bool
prop_init xs = null xs || init xs == init' xs

initSafe :: [a] -> Maybe [a]
initSafe [] = Nothing
initSafe (x:xs) = case initSafe xs of
    Just ys -> Just (x : ys) -- ys is suffix without last element
    Nothing -> Just [] -- xs is empty list, x was last element
prop_initsafe :: Eq a => [a] -> Bool
prop_initsafe xs = initSafe xs == if null xs then Nothing else Just (init xs)

-- 2.9
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = [] 
zip' _ [] = [] 
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
prop_zip :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_zip xs ys = zip' xs ys == zip xs ys

-- 2.10
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = reverse' xs == reverse xs

-- 2.11
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
-- from here on, xs has >= 2 elements:
intersperse' val (x:xs) = x : val : intersperse' val xs 
prop_intersperse :: Eq a => a -> [a] -> Bool
prop_intersperse val xs = intersperse' val xs == intersperse val xs

-- 2.12 
--- OK

-- 1.4
return []
main :: IO Bool
main = $quickCheckAll