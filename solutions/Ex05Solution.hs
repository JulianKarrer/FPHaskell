{-# LANGUAGE TemplateHaskell #-}

module Ex05Solution where

import Test.QuickCheck
import qualified Data.List as L

-- Exercise 1.2 ----------------------------------------------------------------

data AVLTree a = Leaf | Branch Int (AVLTree a) a (AVLTree a)
  deriving (Show, Eq)

height :: AVLTree a -> Int
height Leaf = 0
height (Branch h _ _ _) = h

height' :: AVLTree a -> Int
height' Leaf = 0
height' (Branch _ l _ r) = 1 + max (height' l) (height' r)

branch :: AVLTree a -> a -> AVLTree a -> AVLTree a
branch l x r = Branch (1 + max (height l) (height r)) l x r

-- `Arbitrary` instance for binary search trees

genBST :: (Arbitrary a, Ord a) => Gen (AVLTree a)
genBST = do
  xs <- arbitrary
  genBSTFrom (L.nub (L.sort xs))

genBSTFrom :: [a] -> Gen (AVLTree a)
genBSTFrom sxs = do
  let n = length sxs
  m <- if n `mod` 2 == 1 then do
         return $ n `div` 2
       else do
         coin <- arbitrary
         return $ n `div` 2 - if coin then 1 else 0
  case splitAt m sxs of
    (ls, x:rs) -> do
      l <- genBSTFrom ls
      r <- genBSTFrom rs
      return $ branch l x r
    _ -> return Leaf

instance (Arbitrary a, Ord a) => Arbitrary (AVLTree a) where
  arbitrary = genBST
  shrink Leaf           = []
  shrink (Branch _ l _ r) = [l, r]

-- Properties of binary search trees

isOrdered :: Ord a => AVLTree a -> Bool
isOrdered = isOrderedList . toList where
  isOrderedList :: Ord a => [a] -> Bool
  isOrderedList xs = and $ zipWith (<=) xs (tail xs)

  -- Alternatively:
  --
  -- isOrderedList :: Ord a => [a] -> Bool
  -- isOrderedList [] = True 
  -- isOrderedList [_] = True 
  -- isOrderedList (x : y : xs) = x <= y && isOrderedList (y : xs)

isUnique :: Ord a => AVLTree a -> Bool
isUnique = isUniqueList . toList where
  isUniqueList :: Eq a => [a] -> Bool
  isUniqueList xs = xs == L.nub xs 

isBalanced :: Ord a => AVLTree a -> Bool
isBalanced Leaf = True
isBalanced (Branch _ l _ r) =
  isBalanced l &&
  isBalanced r &&
  abs (height l - height r) <= 1

isLabelledCorrectly :: Ord a => AVLTree a -> Bool
isLabelledCorrectly Leaf = True
isLabelledCorrectly (Branch h l _ r) =
  isLabelledCorrectly l &&
  isLabelledCorrectly r &&
  h == 1 + max (height' l) (height' r)

-- Property tests for the `Arbitrary` instance

prop_genBST_balanced :: AVLTree Int -> Bool
prop_genBST_balanced = isBalanced

prop_genBST_ordered :: AVLTree Int -> Bool
prop_genBST_ordered = isOrdered

prop_genBST_unique :: AVLTree Int -> Bool
prop_genBST_unique = isUnique

prop_genBST_labelledCorrectly :: AVLTree Int -> Bool
prop_genBST_labelledCorrectly = isLabelledCorrectly

-- Combination of the above three properties.
--
-- We use `conjoin` and `counterexample` to combine the properties
-- in such a way, that if the combined property fails, then QuickCheck will
-- tell us which sub-property failed, i.e. whether the tree
-- was (a) not balanced, (b) not ordered, or (c) not unique,
-- or whether (d) the height annotations are incorrect.
--
-- This is particulary useful for testing the functions on AVL trees
-- from the next exercise, where we can reuse this property. (See below)
prop_hasAVLInvariants :: AVLTree Int -> Property
prop_hasAVLInvariants t = conjoin
  [ counterexample "is not balanced" $ isBalanced t
  , counterexample "is not ordered" $ isOrdered t
  , counterexample "is not unique" $ isUnique t
  , counterexample "is not labelled correctly" $ isLabelledCorrectly t
  ]

-- Exercise 1.3 ----------------------------------------------------------------

prop_insert_invariants :: Int -> AVLTree Int -> Property
prop_insert_invariants x t = prop_hasAVLInvariants $ insert x t

prop_inserts_invariants :: [Int] -> AVLTree Int -> Property
prop_inserts_invariants xs t = prop_hasAVLInvariants $ foldr insert t xs

-- Unique, sorted lists
newtype USList a = USList [a] deriving (Show, Eq)

instance (Arbitrary a, Ord a) => Arbitrary (USList a) where
  arbitrary = do
    xs <- arbitrary
    return $ USList $ L.nub $ L.sort xs

prop_fromList_invariants :: USList Int -> Property
prop_fromList_invariants (USList xs) =
  prop_hasAVLInvariants $ fromList xs

prop_merge_invariants :: AVLTree Int -> AVLTree Int -> Property
prop_merge_invariants t1 t2 = prop_hasAVLInvariants $ merge t1 t2

prop_insert_contains :: Int -> AVLTree Int -> Bool
prop_insert_contains x t = contains x $ insert x t

prop_to_from_list :: USList Int -> Bool
prop_to_from_list (USList xs) = toList (fromList xs) == xs

prop_insert_insert :: USList Int -> Int -> Property
prop_insert_insert (USList xs) x =
  (x `notElem` xs) ==>
  toList (insert x (fromList xs)) === L.insert x xs

prop_merge_insert_left :: Int -> AVLTree Int -> Bool
prop_merge_insert_left x t =
  toList (insert x t) == toList (merge (single x) t)

prop_merge_insert_right :: Int -> AVLTree Int -> Bool
prop_merge_insert_right x t =
  toList (insert x t) == toList (merge t (single x))

-- Exercise 1.4 ----------------------------------------------------------------

single :: a -> AVLTree a
single x = Branch 1 Leaf x Leaf

left :: AVLTree a -> AVLTree a
left Leaf = Leaf
left (Branch _ l _ _) = l

right :: AVLTree a -> AVLTree a
right Leaf = Leaf
right (Branch _ _ _ r) = r

hasBalancedRoot :: AVLTree a -> Bool
hasBalancedRoot Leaf = True
hasBalancedRoot (Branch _ l _ r) = abs (height l - height r) <= 1

value :: AVLTree a -> a
value Leaf             = error "value not defined on Leaf"
value (Branch _ _ x _) = x

rotate :: (Ord a, Num a) => AVLTree a -> AVLTree a
rotate Leaf = Leaf
rotate (Branch _ l x r)
  | not (hasBalancedRoot l) = branch (rotate l) x r
  | not (hasBalancedRoot r) = branch l x (rotate r)
  | height l + 1 < height r && height (left r) < height (right r) = -- SR RR
      branch (branch l x (left r))
             (value r)
             (right r)
  | height r + 1 < height l && height (right l) < height (left l) = -- SR LL
      branch (left l)
             (value l)
             (branch (right l) x r)
  | height l + 1 < height r && height (left r) > height (right r) = -- DR RL
      branch (branch l x (left (left r)))
             (value (left r))
             (branch (right (left r)) (value r) (right r))
  | height r + 1 < height l && height (right l) > height (left l) = -- DR LR
      branch (branch (left l) (value l) (left (right l)))
             (value (right l)) 
             (branch (right (right l)) x r)
  | otherwise = branch l x r

-- rotate :: (Ord a, Num a) => AVLTree a -> AVLTree a
-- rotate Leaf = Leaf
-- rotate (Branch _ l x r) | not (hasBalancedRoot l) = branch (rotate l) x r
-- rotate (Branch _ l x r) | not (hasBalancedRoot r) = branch l x (rotate r)
-- rotate (Branch _ l x (Branch rh rl rx rr))
--   | height l + 1 < rh && height rl < height rr = -- SR RR
--       branch (branch l x rl) rx rr
--   | height l + 1 < rh && height rl > height rr = -- DR RL
--       branch (branch l x (left rl))
--              (value rl)
--              (branch (right rl) rx rr)
-- rotate (Branch _ (Branch lh ll lx lr) x r)
--   | height r + 1 < lh && height lr < height ll = -- SR LL
--       branch ll lx (branch lr x r)
--   | height r + 1 < lh && height lr > height ll = -- DR LR
--       branch (branch ll lx (left lr))
--              (value lr) 
--              (branch (right lr) x r)
-- rotate (Branch _ l x r) = branch l x r

insert :: (Num a, Ord a) => a -> AVLTree a -> AVLTree a
insert x Leaf = single x
insert x (Branch h l y r) = case compare x y of
  LT -> rotate $ branch (insert x l) y r
  GT -> rotate $ branch l            y (insert x r)
  EQ -> Branch h l y r

contains :: Ord a => a -> AVLTree a -> Bool
contains _ Leaf = False
contains x (Branch _ l y r) = case compare x y of
  LT -> contains x l
  EQ -> True
  GT -> contains x r

foldTree :: (Int -> b -> a -> b -> b) -> b -> (AVLTree a -> b)
foldTree _ z Leaf = z
foldTree f z (Branch h l x r) = f h (foldTree f z l) x (foldTree f z r)

toList :: AVLTree a -> [a]
toList = foldTree (\_ l x r -> l ++ [x] ++ r) []

fromList :: [a] -> AVLTree a
fromList xs = case splitAt (length xs `div` 2) xs of
  (ls, x:rs) -> branch (fromList ls) x (fromList rs)
  _          -> Leaf

mergeUSLists :: Ord a => [a] -> [a] -> [a]
mergeUSLists []       ys       = ys
mergeUSLists xs       []       = xs
mergeUSLists (x : xs) (y : ys) = case compare x y of 
  LT -> x : mergeUSLists xs (y : ys)
  EQ -> mergeUSLists xs (y : ys)
  GT -> y : mergeUSLists (x : xs) ys

merge :: Ord a => AVLTree a -> AVLTree a -> AVLTree a
merge t1 t2 = fromList (mergeUSLists (toList t1) (toList t2))

isLeaf :: AVLTree a -> Bool
isLeaf Leaf = True
isLeaf _    = False

showTreePretty :: Show a => AVLTree a -> String
showTreePretty t = sep ++ "\n" ++ f 0 t ++ "\n" ++ sep where
  sep = "––––––––––"
  indent i = replicate (i*2) ' '
  f _ Leaf = ""
  f i (Branch _ l x r) =
    (if not (isLeaf l) then f (i + 1) l ++ "\n" else "") ++
    indent i ++ show x ++
    (if not (isLeaf r) then "\n" ++ f (i + 1) r else "")

-- QuickCheck ------------------------------------------------------------------

-- The following code uses TemplateHaskell to define a function `runTests`,
-- which collects all definitions of this module, which start with `prop_`,
-- and runs them with QuickCheck. TemplateHaskell is Haskell's way of metaprogramming,
-- i.e. the $quickCheckAll is a Haskell function, which runs at compile time
-- and generates additional Haskell code, which is then used during the actual compilation.

return []
runTests :: IO Bool
runTests = $quickCheckAll
