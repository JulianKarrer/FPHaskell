{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Main where
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = do
    quickCheck (prop_ordered :: AVLTree Int -> Bool)
    quickCheck (prop_correct_height :: AVLTree Int -> Bool)
    quickCheck (prop_balanced :: AVLTree Int -> Bool)
    quickCheck (prop_unique :: AVLTree Int -> Bool)

data AVLTree a = Leaf | Branch Int (AVLTree a) a (AVLTree a) deriving (Show)

-- EXERCISE 4

insert :: Ord a => a -> AVLTree a -> AVLTree a
insert = ins where
    -- not how the order of t1<t2<t3 remains and so does the order li<i<ri
    rotateRight :: AVLTree a -> AVLTree a
    rotateRight (Branch h (Branch lh t1 li t2) i t3) =
        let subh = (getH t2 `incmax` getH t3) in
        let sub = Branch subh t2 i t3 in
        Branch (subh `incmax` getH t1) t1 li sub
    rotateLeft :: AVLTree a -> AVLTree a
    rotateLeft (Branch h t1 i (Branch hr t2 ri t3)) =
        let subh = getH t1 `incmax` getH t2 in
        let sub = Branch subh t1 i t2 in
        Branch (subh `incmax` getH t3) sub ri t3
    -- getter function for the cached height of a tree
    getH :: AVLTree a -> Int
    getH Leaf = 0
    getH (Branch h _ _ _) = h
    -- operator that calculates a node's height from the 
    -- heights of the left and right child trees
    incmax :: Int -> Int -> Int
    incmax x y = 1 + max x y
    -- re-balance the tree if the height of left and right subtrees 
    -- differs by more than one
    balance :: Ord a => AVLTree a -> a -> AVLTree a
    balance node@(Branch h l i r) elem =
        case (getH l - getH r, node) of
            -- left heavy tree
            (bf, Branch h nodeL@(Branch lh ll li lr) i r) | bf > 1 ->
                if elem < li then
                    rotateRight node
                else
                    let newL = rotateLeft nodeL in
                    rotateRight (Branch (getH newL `incmax` getH r) newL i r)
            -- right heavy tree
            (bf, Branch h l i nodeR@(Branch rh rl ri rr)) | bf < -1 ->
                if elem > ri then
                    rotateLeft node
                else
                    let newR = rotateRight nodeR in
                    rotateLeft (Branch (getH l `incmax` getH newR) l i newR)
            -- balanced tree
            (_, _) -> node
    -- insert function
    ins :: Ord a => a -> AVLTree a -> AVLTree a
    ins elem Leaf = Branch 1 Leaf elem Leaf
    ins elem node@(Branch h l ref r)
      | elem < ref = let newL = ins elem l in
                     balance (Branch (getH newL `incmax` getH r) newL ref r) elem
      | elem == ref = node -- don't insert anything if already in the tree -> uniqueness
      | otherwise = let newR = ins elem r in
                    balance (Branch (getH l `incmax` getH newR) l ref newR) elem



contains :: Ord a => a -> AVLTree a -> Bool
contains x Leaf = False
contains x (Branch h l elem r) = x==elem || contains x l || contains x r

merge :: Ord a => AVLTree a -> AVLTree a -> AVLTree a
merge t1 t2 = fromList $ toList t1 ++ toList t2  -- INEFFICIENT?

toList :: AVLTree a -> [a]
toList Leaf = []
toList (Branch h l elem r) = toList l ++ (elem : toList r)

-- Assumes the list is sorted and all elements are unique.
fromList :: Ord a => [a] -> AVLTree a   -- INEFFICIENT
fromList = foldr insert Leaf

-- EXERCISE 2
eq :: Ord a => a -> a -> Bool
eq x y = (x >= y) && (y >= x)

uniquify :: Ord a => [a] -> [a]
uniquify [] = []
uniquify [x] = [x]
uniquify (x1:rest@(x2:xs)) | eq x1 x2 = uniquify rest
uniquify (x1:xs) = x1 : uniquify xs

instance (Ord a, Arbitrary a) => Arbitrary (AVLTree a) where
    arbitrary :: Gen (AVLTree a)
    -- arbitrary = arbitrary >>= (return fromList . uniquify . sort)
    arbitrary = do
        xs <- arbitrary
        return . fromList . uniquify . sort $ xs


prop_ordered :: Ord a => AVLTree a -> Bool
prop_ordered = ordered . toList where
    ordered :: Ord a => [a] -> Bool
    ordered [] = True
    ordered [x] = True
    ordered (x1:(x2:rest)) = (x1 <= x2) && ordered rest

prop_correct_height :: AVLTree a -> Bool
prop_correct_height Leaf = True
prop_correct_height node@(Branch h l _ r) =
    h == height node  &&
    prop_correct_height l &&
    prop_correct_height r
    where
        height Leaf = 0
        height (Branch _ l _ r) = 1 + max (height l) (height r)

prop_balanced :: AVLTree a -> Bool
prop_balanced Leaf = True
prop_balanced (Branch h l _ r) =
    abs (height l - height r) <= 1
    && prop_balanced l
    && prop_balanced r
    where
        height Leaf = 0
        height (Branch _ l _ r) = 1 + max (height l) (height r)

prop_unique :: Ord a => AVLTree a -> Bool
prop_unique t = uniquify (toList  t) == toList t

prop_all :: Ord a => AVLTree a -> Bool
prop_all x = prop_ordered x && prop_correct_height x && prop_balanced x && prop_unique x

-- test stubs

prop_fromList :: (Ord a) => [a] -> Bool
prop_fromList xs = prop_all $ fromList xs

