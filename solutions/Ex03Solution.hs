module Ex03Solution where

import Data.List

-- Composition (.) and Appliction ($) ------------------------------------------

-- Avoiding parentheses

f' :: [Int] -> Int
f' xs = sum (filter (>0) (map (*2) xs))

f1 :: [Int] -> Int
f1 xs = sum $ filter (>0) $ map (*2) xs

f2 :: [Int] -> Int
f2 xs = sum . filter (>0) . map (*2) $ xs

f3 :: [Int] -> Int
f3 = sum . filter (>0) . map (*2)

-- Composing composition with composition has the following type

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(.:) = (.).(.)

-- The meaning of this operation is composing a function with two parameters
-- in front of a function with one parameter.

-- In other words, (.:) behaves identical to the following function:

(..:) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(f ..: g) x y = f (g x y)

-- This can be seen by eta-expanding the definition of (.:) and
-- then reducing as followed:

--   (f .: g) x y
-- = ((.).(.)) f g x y
-- = (.) (f .) g x y
-- = ((f .) . g) x y
-- = (f .) (g x) y
-- = (f . g x) y
-- = f (g x y)
-- = (f ..: g) x y

-- Lazy Evaluation -------------------------------------------------------------

-- cycle and iterate

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)

-- foldl vs foldr

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl _ z []       = z
-- foldl f z (x : xs) = foldl f (f z x) xs
--
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []       = z
-- foldr f z (x : xs) = f x (foldr f z xs)

-- Using `foldl` on infinite lists will never terminate, because
-- if `foldl` is called on a non-empty list it reduces to another
-- `foldl` call, so a final result can only be obtained after
-- traversing the whole list.
--
-- Using `foldr` on infinite lists might terminate, because 
-- if `foldr` is called on a non-empty list it reduces to
-- `f x (foldr f z xs)` and if `f` does not look at its second
-- argument then the `(foldr f z xs)` will never be evaluated.
-- An (artifical) example is shown below:

example' = foldr (\x acc -> 42) 666 [0..]

-- This function call reduces as follows:
--
--   foldr' (\x acc -> 42) 666 [0..]
-- = (\x acc -> 42) 0 (foldr' (\x acc -> 42) 666 [1..])
-- = 42
--
-- Recall that with lazy evaluation the arguments to a function are not
-- evaluated before the function is called, but only when the function
-- actually uses its parameter to produce the result, which for (\x acc -> 42)
-- is not the case.
--
-- A less artifical example is to define the `takeWhile` function
-- with foldr'. The `takeWhile` function takes a predicate and a list
-- as arguments (like `filter`) and returns all elements from the
-- list until the first element is reached where the predicate is not true
-- anymore, e.g. `takeWhile (<3) [0..]` evaluates to `[0,1,2]`.
-- The `takeWhile` function can be implemented with `foldr` as follows:

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if f x then x : acc else []) []

-- and indeed terminates if called with the right arguments

example'' = takeWhile' (<10) [0..]

-- Worst-Case Asymptotic Runtimes

-- (a)
f1' :: [Int] -> Int
f1' = head . map (+1)
-- This function runs in constant time. Example reduction:
--
--   f1' (42 : longList)
-- = head (map (+1) (42 : longList))
-- = head ((42 + 1) : (map (+1) longList))
-- = 42 + 1
-- = 43

-- (b)
f2' :: [Int] -> Int
f2' = last . map (+1)
-- This function runs in linear time, because to retrieve the last element of a
-- single-linked-list the whole list needs to be traversed. However, the mapping
-- of the (+1) function is only evaluated for the last element, and not for the other
-- elements:
--   f2' (0 : 1 : 2 : [])
-- = last (map (+1) (0 : 1 : 2 : []))
-- = last ((0 + 1) : map (+1) (1 : 2 : []))
-- = last (map (+1) (1 : 2 : []))
-- = last ((1 + 1) : map (+1) (2 : []))
-- = last (map (+1) (2 : []))
-- = last ((2 + 1) : map (+1) [])
-- = last ((2 + 1) : [])
-- = 2 + 1
-- = 3

-- Hamming Stream

(<++>) :: [Integer] -> [Integer] -> [Integer]
(x:xs) <++> (y:ys)
  | x < y     = x : (xs <++> (y:ys))
  | x > y     = y : ((x:xs) <++> ys)
  | otherwise = x : (xs <++> ys)
_ <++> _ = error "merge not intended to be called on finite lists"

infixr 4 <++>

hamming :: [Integer]
hamming = 1 : (map (* 2) hamming <++> map (* 3) hamming <++> map (* 5) hamming)
















