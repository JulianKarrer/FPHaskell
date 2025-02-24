-- OCTOBER 18 -----------------------------------------------------------------

-- Actual types: Uppercase, Variables/Type variables lowercase
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

test1 = add_uncurried(2,3)
test2 = add_uncurried2(2,3)

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
