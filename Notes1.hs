-- OCTOBER 18 -----------------------------------------------------------------

-- Actual types: Uppercase, Variables/Type variables lowercase
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
x::Int
x = 42

id :: forall a. (a -> a)
id a = a

-- class: Type Classes = Interfaces, Rust: Traits

-- MONAAAAAAAAADS

main :: IO ()
main = putStrLn "Hello World!"

-- FUN! ctions

add :: Integer -> Integer -> Integer
add = \x -> \y -> x+y

add_uncurried :: (Int, Int) -> Int
add_uncurried = \xy -> fst xy + snd  xy

add_uncurried2 :: (Integer, Integer) -> Integer
add_uncurried2 = \(x,y) -> x + y

test1 = add_uncurried (2,3)
test2 = add_uncurried2 (2,3)

inc = (1+)


-- OCTOBER 22 -----------------------------------------------------------------

-- use cabal file otherModules: subdirectory.newfile 
-- and in the newfile: module subdirectory.newfile where


-- recursive funciton definition: like pattern matching, 
-- define in pieces for each case
power :: Integer -> Integer -> Integer

--power _ 0 = 1
-- power x n = if n > 0 then x * power x (n-1) else error "Negative Integer exponent is poopy"

-- pattern guard cases are matched top to bottom, otherwise === True
power x n
    | n==0 = 1
    | n>0 = x * power x (n-1)
    | otherwise = error "Negative Integer exponent not supported"


-- bascially an enum: (rgb consists of three ints)
data Colour = Red | Green | Blue | Rgb Int Int Int
-- derive the most obvious show, just the name 
-- derive equality
-- create an order according to the constructor order (red <= green <= blue)
    deriving (
        Show,
        Eq, Ord)

-- or implement show yourself
-- instance Show Colour where
--     show Red = "r"
--     show Green = "g"
--     show Blue = "b"

-- OCTOBER 29 -----------------------------------------------------------------

data BTree a = Leaf | Branch { left::BTree a, item::a, right::BTree a}

height :: BTree a -> Int
height Leaf = 0
height (Branch l _ r) = 1 + max (height l) (height r)

-- record match
height' :: BTree a -> Int
height' Leaf = 0
height' (Branch {left = l, right = r}) = 1 + max (height' l) (height' r)


sumTree :: Num a => BTree a -> a
sumTree Leaf = 0
sumTree (Branch l i r) = i + sumTree l + sumTree r

---
-- assume BTree a where Ord a is a binary search tree

-- node :: a -> BTree a 

-- insert :: Ord a => BTree a -> a -> BTree a
-- insert Leaf x


-------

primes :: [Integer]
primes = 1 : sieve [2..]
sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]


------

data BTreee = Leafy Int | Branchy BTreee BTreee deriving Show

minTree :: BTreee -> BTreee
minTree t = let (mini, constr) = minTree' t in
  constr mini

minTree' :: BTreee -> (Int, Int -> BTreee)
minTree' (Leafy i) = (i, Leafy)
minTree' (Branchy l r) =
  let (lmin, lconstr) = minTree' l in
  let (rmin, rconstr) = minTree' r in
  (min lmin rmin, \n -> Branchy (lconstr n) (rconstr n))

exampleBTree :: BTreee
exampleBTree = Branchy (Branchy (Branchy (Leafy 4) (Leafy 9)) (Leafy 2)) (Branchy (Leafy (-1)) (Leafy 6))

------


-- orderedList :: (Arbitrary a, Ord a) => Gen [a]
-- orderedList = sort <$> arbitrary

-- sampled_ordered :: IO ()
-- sampled_ordered = sample (orderedList::Gen [Int])

--------

-- transpose :: [[a]] -> [[a]]
-- transpose = foldr (\ xs -> zipWith ($) ((:) <$> xs)) (repeat [])

-- ALTERNATIVE LIST APPLICATIVE
data AList a = ANil | (:+) a (AList a)

instance Functor AList where
  fmap f  ANil = ANil
  fmap f (x :+ xs) = f x :+ fmap f xs

instance Applicative AList where
  pure x = x :+ pure x -- == repeat on AList
  fs <*> xs =  aZipWith ($) fs xs where
    aZipWith :: (a->b->c) -> AList a -> AList b -> AList c
    aZipWith f (x :+ xs) (y :+ ys) = f x y :+ aZipWith f xs ys
    aZipWith _ _ _ = ANil

transpose :: AList (AList a) ->  AList (AList a)
transpose ANil = pure ANil
transpose (x :+ xs) = ((:+) <$> x) <*> transpose xs


----- 
-- are compositions of functors again functors? (yes)
-- what about monads (no -> transformers)
-- what about applicatives? (also yes)

newtype Comp f g a = Comp (f (g a))
instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Comp f g a -> Comp f g b
  fmap h (Comp fga) = Comp $ fmap (fmap h) fga

instance (Applicative f, Applicative g) => Applicative (Comp f g) where  
  pure x = Comp $ pure $ pure x
  Comp fga_b <*> Comp fga = Comp $ fmap (<*>) fga_b <*> fga


