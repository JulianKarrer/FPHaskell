-- Required for QuickCheck (see end of this file)
{-# LANGUAGE TemplateHaskell #-}

module Ex01Solution where

import Data.List (sort)
import Test.QuickCheck

-- Part 1: Warmup --------------------------------------------------------------

mini :: Integer -> Integer -> Integer
mini x y = if x < y then x else y

-- alternative with pattern guards
mini' :: Integer -> Integer -> Integer
mini' x y | x < y     = x
          | otherwise = y

maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x, y, z) = maxi x (maxi y z)

med :: Integer -> Integer -> Integer -> Integer
med x y z = max3 (mini x y) (mini y z) (mini x z)

-- QuickCheck Tests

prop_mini :: Integer -> Integer -> Bool
prop_mini x y = mini x y == head (sort [x, y])

prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = maxi x y == last (sort [x, y])

prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max3 x y z == last (sort [x, y, z])

prop_max3Tupled :: Integer -> Integer -> Integer -> Bool
prop_max3Tupled x y z = max3Tupled (x, y, z) == last (sort [x, y, z])

prop_med :: Integer -> Integer -> Integer -> Bool
prop_med x y z = med x y z == sort [x, y, z] !! 1

-- Part 2: Lists ---------------------------------------------------------------

null' :: [a] -> Bool
null' [] = True
null' _  = False

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- alternative with foldr:
sum'' :: [Integer] -> Integer
sum'' = foldr (+) 0

concat' :: [[a]] -> [a]
concat' []         = []
concat' (xs : xss) = xs ++ concat' xss

-- alternative with foldr:
concat'' :: [[a]] -> [a]
concat'' = foldr (++) []

elem' :: Integer -> [Integer] -> Bool
elem' _ []                   = False
elem' x (y : ys) | x == y    = True
                 | otherwise = elem' x ys

take' :: Integer -> [a] -> [a]
take' n (x : xs) | n > 0 = x : take' (n - 1) xs
take' _ _                = []

drop' :: Integer -> [a] -> [a]
drop' n (_ : xs) | n > 0 = drop' (n - 1) xs
drop' _ xs               = xs

last' :: [a] -> a
last' []       = error "last' is undefined on the empty list"
last' (x : []) = x
last' (_ : xs) = last' xs

lastSafe :: [a] -> Maybe a
lastSafe []       = Nothing
lastSafe (x : []) = Just x
lastSafe (_ : xs) = lastSafe xs

init' :: [a] -> [a]
init' []       = error "init' is undefined on the empty list"
init' (_ : []) = []
init' (x : xs) = x : init' xs

initSafe :: [a] -> Maybe [a]
initSafe []       = Nothing
initSafe (_ : []) = Just []
initSafe (x : xs) = case initSafe xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

-- alternative using `fmap`, which maps a function over the elements of a Maybe value.
-- (Think of Maybe as a list which always has exactly 0 or 1 elements.)
initSafe' :: [a] -> Maybe [a]
initSafe' []       = Nothing
initSafe' (_ : []) = Just []
initSafe' (x : xs) = fmap (x:) (initSafe' xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- faster alternative: reverse' runs in O(n²), whereas reverse'' runs in O(n)
reverse'' :: [a] -> [a]
reverse'' xs = f xs [] where
  f []       ys = ys
  f (x : xs) ys = f xs (x : ys)

zip' :: [a] -> [b] -> [(a, b)]
zip' []       _        = []
zip' _        []       = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

intersperse' :: a -> [a] -> [a]
intersperse' _ []       = []
intersperse' _ (y : []) = y : []
intersperse' x (y : ys) = y : x : intersperse' x ys

-- QuickCheck ------------------------------------------------------------------

-- The following code uses TemplateHaskell to define a function `runTests`,
-- which collects all definitions of this module, which start with `prop_`,
-- and runs them with QuickCheck. TemplateHaskell is Haskell's way of metaprogramming,
-- i.e. the $quickCheckAll is a Haskell function, which runs at compile time
-- and generates additional Haskell code, which is then used during the actual compilation.

return []
runTests :: IO Bool
runTests = $quickCheckAll
