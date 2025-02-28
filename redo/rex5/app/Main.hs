{-# LANGUAGE TemplateHaskell, InstanceSigs #-}
module Main where
import Data.List (nub, sort)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck ( chooseInt, listOf, quickCheckAll, Arbitrary(arbitrary) )


data AVLTree a = Leaf | Branch Int (AVLTree a) a (AVLTree a) deriving Show

-- Utilities
removeFirst :: (Eq t, Num t) => t -> [a] -> [a]
removeFirst 0 ys = ys
removeFirst i (_:ys) = removeFirst (i-1) ys
removeFirst _ _ = []

keepLast :: Int -> [a] -> [a]
keepLast n xs = removeFirst (length xs - n) xs

height :: (Num a1, Ord a1) => AVLTree a2 -> a1
height Leaf = 0
height (Branch _ l _ r) = 1 + max (height l) (height r)

pureT :: a -> AVLTree a
pureT x = Branch 1 Leaf x Leaf

getMax :: (Bounded a, Ord a) => AVLTree a -> a
getMax Leaf = minBound
getMax (Branch _ l i r) = getMax l `max` i `max` getMax r

getMin :: (Bounded a, Ord a) => AVLTree a -> a
getMin Leaf = maxBound
getMin (Branch _ l i r) = getMin l `min` i `min` getMin r


-- 1.2
instance (Arbitrary a, Ord a) => Arbitrary (AVLTree a) where
  arbitrary :: (Arbitrary a, Ord a) => Gen (AVLTree a)
  arbitrary = do
    xs <- listOf arbitrary
    let xs' = nub $ sort xs
    (t, _) <- genFromListG xs'
    return t

genFromListG :: [a] -> Gen (AVLTree a, Int)
genFromListG [] = return (Leaf, 0)
genFromListG [x] = return (pureT x, 1)
genFromListG [l, r] = do
  coinFlip <- arbitrary
  if coinFlip then
    return (Branch 2 (pureT l) r Leaf, 2)
  else
    return (Branch 2 Leaf l (pureT r), 2)
genFromListG xs = do
  let size = length xs
  splitIndex <- if even size then do
    coinFlip <- chooseInt (0,1)
    return $ size `div` 2 - coinFlip
  else
    return $ size `div` 2
  let middle = xs !! splitIndex
  (l, hl) <- genFromListG $ take splitIndex xs
  (r, hr) <- genFromListG $ removeFirst (splitIndex+1) xs
  let h = 1 + max hl hr
  return (Branch h l middle r, h)


-- AVLTree properties
prop_balanced :: AVLTree Int -> Bool
prop_balanced Leaf = True
prop_balanced (Branch _ l _ r) =
  (abs (height l - height r) <= 1 ) &&
  prop_balanced l &&
  prop_balanced r

prop_ordered :: AVLTree Int -> Bool
prop_ordered Leaf = True
prop_ordered (Branch _ l i r) = getMax l <= i && i <= getMin r

prop_unique :: AVLTree Int -> Bool
prop_unique Leaf = True
prop_unique t = toList t == nub (toList t)

prop_height_correct :: AVLTree Int -> Bool
prop_height_correct Leaf = True
prop_height_correct (Branch h l _ r) = h == 1 + max (height l) (height r)

prop_avltree :: AVLTree Int -> Bool
prop_avltree t = prop_balanced t && prop_ordered t && prop_unique t && prop_height_correct t


-- 1.3 Function properties
prop_insert :: Int -> AVLTree Int -> Bool
prop_insert x t = prop_avltree $ insert x t
prop_insert_balanced :: Int -> AVLTree Int -> Bool
prop_insert_balanced x t = prop_balanced $ insert x t
prop_insert_ordered :: Int -> AVLTree Int -> Bool
prop_insert_ordered x t = prop_ordered $ insert x t
prop_insert_unique :: Int -> AVLTree Int -> Bool
prop_insert_unique x t = prop_unique $ insert x t
prop_insert_height_correct :: Int -> AVLTree Int -> Bool
prop_insert_height_correct x t = prop_height_correct $ insert x t


prop_insert_contains :: Ord a => a -> AVLTree a -> Bool
prop_insert_contains x t = contains x $ insert x t

prop_contains_elem :: Ord a => [a] -> Bool
prop_contains_elem xs = let xs' = nub $ sort xs in
  all (`contains` fromList xs') xs'

prop_contains_elem2 :: (Eq a, Ord a) => [a] -> [a] -> Bool
prop_contains_elem2 xst xs = let xs' = nub $ sort xst in
  all (\x -> (x `contains` fromList xs') == (x `elem` xs')) xs

prop_merge :: AVLTree Int -> AVLTree Int -> Bool
prop_merge t1 t2 = prop_avltree $ merge t1 t2
prop_merge_contains1 :: Ord a => AVLTree a -> [a] -> Bool
prop_merge_contains1 t1 xs2 =  all (`contains` merge t1 (fromList xs2)) xs2
prop_merge_contains2 :: Ord a => [a] -> AVLTree a -> Bool
prop_merge_contains2 xs1 t2 =  all (`contains` merge (fromList xs1) t2) xs1
prop_merge_contains3 :: Ord a => [a] -> [a] -> Bool
prop_merge_contains3 xs1 xs2 =  all (`contains` merge (fromList xs1) (fromList xs2)) (xs1++xs2)

prop_list_from_to :: Ord a => [a] -> Bool
prop_list_from_to xs = let xs' = nub $ sort xs in
  xs' == toList (fromList xs')


-- 1.4
getHeight :: AVLTree a -> Int
getHeight Leaf = 0
getHeight (Branch h _ _ _ ) = h

rotateRight :: AVLTree a -> AVLTree a
rotateRight (Branch h (Branch _ alpha a beta) b gamma) =
  let hr' = 1 + max (getHeight beta) (getHeight gamma) in
  let h' = 1 + max (getHeight alpha) hr' in
  Branch h' alpha a (Branch hr' beta b gamma)

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft (Branch h t1 i (Branch _ t2 ri t3)) =
  let hl' = 1 + max (getHeight t1) (getHeight t2) in
  let h' = 1 + max hl' (getHeight t3) in
  Branch h' (Branch hl' t1 i t2) ri t3

insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x Leaf = pureT x 
insert x t0@(Branch h l i r) = 
  -- insert x in correct branch
  let t1@(Branch _ l' i' r') = case compare x i of
        EQ -> t0
        LT -> Branch h (insert x l) i r
        GT -> Branch h l i (insert x r)
  in 
  -- compute updated height
  let h' = 1 + max (getHeight l') (getHeight r') in
  let node = Branch h' l' i' r' in
  case bf node of
    2 -> -- left heavy
      if bf l' >= 0 then 
        -- left left
        rotateRight node
      else 
        -- left-right
        let l'' = rotateLeft l' in 
        let h'' = 1 + max (getHeight l'') (getHeight r') in 
        rotateRight (Branch h'' l'' i' r')
    -2 -> -- right heavy
      if bf r' >= 0 then 
        -- right left
        let r'' = rotateRight r' in 
        let h'' = 1 + max (getHeight l') (getHeight r'') in 
        rotateLeft (Branch h'' l' i' r'')
      else 
        -- right-right
        rotateLeft node
    _ -> node
  where
    bf :: AVLTree a -> Int
    bf (Branch _ l _ r) = getHeight l - getHeight r
    bf _ = 0



example :: AVLTree Int
example = Branch 2
    (Branch 1 Leaf 2 Leaf)
    5
    (Branch 1 Leaf 8 Leaf)
example1 :: AVLTree Int
example1 = Branch 2 (Branch 1 Leaf (-3) Leaf) 3 Leaf --insert 0

contains :: Ord a => a -> AVLTree a -> Bool
contains _ Leaf = False
contains x (Branch _ l i r) = case compare x i of
  EQ -> True
  LT -> contains x l
  GT -> contains x r

merge :: Ord a => AVLTree a -> AVLTree a -> AVLTree a
merge t1 t2 = fromList $ sort.nub $ toList t1 ++ toList t2

toList :: AVLTree a -> [a]
toList Leaf = []
toList (Branch _ l i r) = toList l ++ (i : toList r)

-- Assumes the list is sorted and all elements are unique.
fromList :: [a] -> AVLTree a
fromList xs = fst $ genFromList xs where
  -- deterministic version of genFromListG
  genFromList :: [a] -> (AVLTree a, Int)
  genFromList [] = (Leaf, 0)
  genFromList [x] = (pureT x, 1)
  genFromList [l, r] = (Branch 2 (pureT l) r Leaf, 2)
  genFromList xs =
    let size = length xs in
    let split = size `div` 2 in
    let middle = xs !! split in
    let (l, hl) = genFromList $ take split xs in
    let (r, hr) = genFromList $ removeFirst (split+1) xs in
    let h = (1+max hl hr) in
    (Branch h l middle r, h)

-- QUICKCHECK
return []
main :: IO Bool
main = do
    $quickCheckAll