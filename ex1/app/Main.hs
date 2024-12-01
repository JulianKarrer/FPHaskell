-- {-# LANGUAGE TemplateHaskell #-}

module Main where
import Test.QuickCheck.All
import Data.List (sort)
import Data.Maybe (isNothing)
import Test.QuickCheck (quickCheck)

-- EXERCISE 1
-- 1.1
maxi :: Integer -> Integer -> Integer
maxi x y
    | x>=y = x
    | otherwise = y

mini :: Integer -> Integer -> Integer
mini x y
    | x>=y = y
    | otherwise = x

-- 1.2
max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x,y,z) = maxi x (maxi y z)

-- 1.3
med :: Integer -> Integer -> Integer -> Integer
-- med x y z
--     | x == max3 x y z = maxi y z
--     | y == max3 x y z = maxi x z
--     | z == max3 x y z = maxi x y
med x y z = max3 (mini x y) (mini x z) (mini y z)

-- 1.4
prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = maxi x y == max x y

prop_mini :: Integer -> Integer -> Bool
prop_mini x y = mini x y == min x y

prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max3 x y z == sort [x,y,z] !! 2

prop_max3Tupled :: (Integer, Integer, Integer) -> Bool
prop_max3Tupled (x,y,z) = max3Tupled (x,y,z) == last( sort [x,y,z] )

prop_med :: Integer -> Integer -> Integer -> Bool
prop_med x y z = (sort [x,y,z] !! 1 )== med x y z

main :: IO ()
main = do
    quickCheck prop_maxi
    quickCheck prop_mini
    quickCheck prop_max3
    quickCheck prop_max3Tupled
    quickCheck prop_med

-- return []
-- runTests :: IO Bool
-- runTests = $quickCheckAll

-- main :: IO ()
-- main = putStrLn "Hello World"

-- EXERCISE 2
-- 2.1
null' :: [a] -> Bool
null' [] = True
null' (_ : _) = False

-- 2.2
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs


-- 2.3
concat' :: [[a]] -> [a]
concat' [] = []
-- concat' [x] = x
concat' (x : xs) = x ++ concat' xs

-- 2.4
-- elem' :: Integer -> [Integer] -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (head : rest) = head==x || elem' x rest

-- 2.5
-- takes UP TO the specified amount instead of raising an error
take' :: Integer -> [a] -> [a]
-- take' 0 xs = []
-- take' n [] = []
-- take' n (head : rest) = head : take' (n-1)  rest

take' n (x:xs) | n>0 = x : take' (n-1) xs
take' _ _ = []

-- 2.6
-- returns [] if the number of elements dropped exceeds the list length
drop' :: Integer -> [a] -> [a]
-- drop' 0 xs = xs
-- drop' n [] = []
-- drop' n (head : rest) = drop' (n-1) rest
drop' n (x:xs) | n>0 = drop' (n-1) xs
drop' _ xs = xs


-- 2.7
last' :: [a] -> a
last' [] = error "Called last' on an empty list"
last' [x] = x
last' (head:rest) = last' rest

lastSafe' :: [a] -> Maybe a
lastSafe' [] = Nothing
lastSafe' [x] = Just x
lastSafe' (head : rest) = lastSafe' rest

-- 2.8
init' :: [a] -> [a]
init' [] = error "Called init' on an empty list"
init' [x] = []
init' (head:rest) = head : init' rest

initSafe' :: [a] -> Maybe [a]
initSafe' [] = Nothing
initSafe' [_] = Just []
initSafe' (x:xs) = fmap (x:) (initSafe' xs)
-- initSafe' (h : r) = let next = initSafe' r in
--     case next of
--         Just x -> Just (h : x)
-- alternatively:
-- initSafe' (h : r) = case initSafe' r of
--     Nothing -> Nothing
--     Just ys -> Just (h : ys)

-- 2.9
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- 2.10
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:r) = reverse' r ++ [h]

-- the following uses linear runtime instead
revlin  :: [a] -> [a]
revlin xs = helper xs [] where
    helper [] ys = ys
    helper (x:xs) ys = helper xs (x:ys)

-- 2.11
intersperse' :: a -> [a] -> [a]
intersperse' elem [] = []
intersperse' elem [x] = [x]
intersperse' elem (x:xs) = x : elem : intersperse' elem xs

-- intercalate : same shit with lists, ie. intercalate " abc " ["x", "y", "z"] = "x abc y abc z"