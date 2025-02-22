module Main where
import Data.Maybe (isNothing)
-- import Test.QuickCheck

main :: IO ()
-- main = quickCheck prop_f
main = putStrLn "hi"

-- EXERCISE 1
--1.1
f :: [Int] -> Int
f xs = sum (filter (>0) (map (*2) xs))

f' :: [Int] -> Int
f' xs = sum $ filter (>0)  $ map (*2) xs

f'' :: [Int] -> Int
f'' xs = sum . filter (>0)  $ map (*2) xs

f''' :: [Int] -> Int
f''' = sum . filter (>0) . map (*2)

prop_f :: [Int] -> Bool
prop_f xs = f xs == f' xs &&  f xs == f'' xs &&  f xs == f''' xs

-- 1.2
-- (.:) :: (b->c) -> (a->b) -> (c -> d) -> a -> d
(.:) = (.) . (.) -- (g nach f) nach (g nach f) von f,g

-- compose a binary function with a unary function
(..:) :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
(f ..: g) x y = f (g x y) 


-- EXERCISE 2
--2.1
cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

--2.2
iterate'' :: (a -> a) -> a -> [a]
iterate'' f init = init : map f (iterate'' f init)
-- iterate f x = x : iterate f (f x)

--2.3
terminatesYay :: Integer
terminatesYay = foldr const 0 [0..]

-- foldl never terminates on infinite lists since it expands to another call and only returns a result when the whole list is traversed
-- foldl: (((0+1)+2)+3)...

-- foldr expands to something like (1 + (2 + ( 3 + ...  + 0))) so if + is replaced by a funciton ? that ignores the second argument, lazy evaluation makes the (1 ? rest) evaluate to ? 1 _ without evaluating "rest"
-- const is an example of ? that ignores arguments, (\x acc -> 42) is another example

--2.4
f1 :: [Integer] -> Integer
f1  = head . map (+1) -- O(1), +1 evaluated once

f2 :: [Integer] -> Integer
f2 = last . map (+1) -- O(N), +1 evaluated N times

-- 2.5
isHam :: Integer -> Bool
isHam n | n == 1 = True
        | otherwise = maybe False isHam (hamReduce n)
        where
        hamReduce :: Integer -> Maybe Integer
        hamReduce x | even x = Just (x `div` 2)
            | x `mod` 3 == 0 = Just (x `div` 3)
            | x `mod` 5 == 0 = Just (x `div` 5)
            | otherwise      = Nothing

hamming :: [Integer]
hamming = filter isHam [1..]

-- always of form: x = 2^n * 3^n * 5^n

-- {1} then multiply by 2,3,5 and union
--  {1,2,3,5}
--  {1, 2*2, 2*3, 2*5 3*2, 3*5} ...

-- make operator <++> that takes (infinite) lists and combines them in order, ignoring duplicates
-- [1,3,5] <++> [1,6,8] = [1,3,5,6,8]
-- use that on the X*2, X*3, X*5 infinite lists