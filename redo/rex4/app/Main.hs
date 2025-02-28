{-# LANGUAGE TemplateHaskell, InstanceSigs, FlexibleInstances #-}
module Main where
import Test.QuickCheck ( quickCheckAll )
import Data.List (intersperse, intercalate, delete)
import Text.Read (readMaybe)


-- EXERCISE 1
class Semigroup' a where
    (<+>) :: a -> a -> a

class Semigroup' a => Monoid' a where
    mempty' :: a

-- 1.1a
-- alternatively: && and True
instance Semigroup' Bool where (<+>) :: Bool -> Bool -> Bool
                               (<+>) = (||)
instance Monoid' Bool where mempty' :: Bool
                            mempty' = False

-- 1.1b
instance Semigroup' Int where (<+>) = (+)
instance Monoid' Int where mempty' = 0

instance Semigroup' Integer where (<+>) = (+)
instance Monoid' Integer where mempty' = 0

-- 1.1c
instance Semigroup' [a] where (<+>) = (++)
instance Monoid' [a] where mempty' = []

-- 1.1d
instance Semigroup' (Maybe a) where
    (Just x) <+> _       = Just x
    Nothing <+> (Just y) = Just y
    _ <+> _              = Nothing
instance Monoid' (Maybe a) where mempty' = Nothing

-- 1.1e
instance (Semigroup' a, Semigroup' b) => Semigroup' (a,b) 
    where (x1,y1) <+> (x2, y2) = (x1<+>x2, y1 <+> y2)
instance (Monoid' a, Monoid' b) => Monoid' (a,b) where 
    mempty' = (mempty', mempty')

--1.1f 
instance (Semigroup' a, Semigroup' b) => Semigroup' (a -> b) where
  (f <+> g) x = f x <+> g x
instance (Monoid' a, Monoid' b) => Monoid' (a->b) where 
  mempty' :: (Monoid' a, Monoid' b) => a -> b
  mempty' = const mempty'


-- TEST 1.1
prop_semi_b :: Bool -> Bool -> Bool -> Bool
prop_semi_b x y z = (x <+> y) <+> z == x <+> (y <+> z)
prop_monoid_b :: Bool -> Bool
prop_monoid_b x = (x <+> mempty' == x) && (mempty' <+> x == x)

prop_semi_i :: Int -> Int -> Int -> Bool
prop_semi_i x y z = (x <+> y) <+> z == x <+> (y <+> z)
prop_monoid_i :: Int -> Bool
prop_monoid_i x = (x <+> mempty' == x) && (mempty' <+> x == x)

prop_semi_l :: Eq a => [a] -> [a] -> [a] -> Bool
prop_semi_l x y z = (x <+> y) <+> z == x <+> (y <+> z)
prop_monoid_l :: Eq a => [a] -> Bool
prop_monoid_l x = (x <+> mempty' == x) && (mempty' <+> x == x)

prop_semi_m :: Eq a => Maybe a -> Maybe a -> Maybe a -> Bool
prop_semi_m x y z = (x <+> y) <+> z == x <+> (y <+> z)
prop_monoid_m :: Eq a => Maybe a -> Bool
prop_monoid_m x = (x <+> mempty' == x) && (mempty' <+> x == x)

prop_semi_t :: (Eq a, Eq b, Semigroup' a, Semigroup' b) => (a, b) -> (a, b) -> (a, b) -> Bool
prop_semi_t x y z = (x <+> y) <+> z == x <+> (y <+> z)
prop_monoid_t :: (Eq a, Eq b, Monoid' a, Monoid' b) => (a,b) -> Bool
prop_monoid_t x = (x <+> mempty' == x) && (mempty' <+> x == x)

instance Show (Integer -> Integer) where
  show :: (Integer -> Integer) -> String
  show = const "function"
prop_semi_f :: (Eq a, Eq b, Semigroup' a, Semigroup' b) => (a -> b) -> (a -> b) -> (a -> b) -> a -> Bool
prop_semi_f x y z a = ((x <+> y) <+> z) a == (x <+> (y <+> z)) a
prop_monoid_f :: (Eq a, Eq b, Monoid' a, Monoid' b) => (a -> b) -> a -> Bool
prop_monoid_f f a = ((f <+> mempty') a == f a) && ((mempty' <+> f) a == f a)

-- 1.2
mconcat' :: (Monoid' m) => [m] -> m
mconcat' = foldr (<+>) mempty'

-- 1.3
class Foldable' f where
    foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' _ cur [] = cur
  foldr' f cur (x:xs) = f x (foldr' f cur xs)

instance Foldable' Maybe where
  foldr' _ cur Nothing = cur
  foldr' f cur (Just x) = f x cur

data BTree a = Leaf | Branch (BTree a) a (BTree a)
instance Foldable' BTree where
    foldr' _ cur Leaf = cur
    foldr' f cur (Branch l i r) = foldr' f (f i (foldr' f cur r)) l

-- 1.4
fold' :: (Foldable' f, Monoid' a) => f a -> a
fold' = foldr' (<+>) mempty'  


-- 1.5
newtype Bool1 = Bool1 { unBool1 :: Bool }
instance Semigroup' Bool1 where b1 <+> b2 = Bool1 $ unBool1 b1 && unBool1 b2
instance Monoid' Bool1 where mempty' = Bool1 True


newtype Bool2 = Bool2 { unBool2 :: Bool }
instance Semigroup' Bool2 where b1 <+> b2 = Bool2 $ unBool2 b1 || unBool2 b2
instance Monoid' Bool2 where mempty' = Bool2 False


newtype Int1 = Int1 { unInt1 :: Int }
instance Semigroup' Int1 where b1 <+> b2 = Int1 $ unInt1 b1 + unInt1 b2
instance Monoid' Int1 where mempty' = Int1 0


newtype Int2 = Int2 { unInt2 :: Int }
instance Semigroup' Int2 where b1 <+> b2 = Int2 $ unInt2 b1 * unInt2 b2
instance Monoid' Int2 where mempty' = Int2 1

-- EXERCISE 2

--2.1a
echoLine :: IO ()
echoLine = 
    getLine >>= \l -> 
    putStrLn l

--2.1b
greet :: IO ()
greet = 
    putStrLn "What's your name" >> 
    getLine >>= \l -> 
    putStrLn ("Hello, " ++ l ++ "!")

-- 2.1c
greetFormal :: IO ()
greetFormal = 
    putStrLn "What's your first name?" >>
    getLine >>= \firstName ->
    putStrLn "What's your surname?" >>
    getLine >>= \surName ->
    let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!" in
    putStrLn greeting

-- 2.1d
choices :: IO ()
choices = 
    putStrLn "Do you want to go outside?" >>
    getLine >>= \l -> 
    if l == "yes" then 
        putStrLn "It rains!" >>
        putStrLn "Too, bad!"
    else
        putStrLn "It still rains! Not so bad!"

-- 2.2
choose :: String -> [String] -> IO String
choose question answers = do
    putStrLn $ question ++ "[" ++ intercalate "|" answers ++ "]"
    l <- getLine
    if l `elem` answers then
        return l
    else do
        putStrLn "Invalid input. Try again!"
        choose question answers

-- 2.3
actions :: [String]
actions = ["add", "display", "remove", "quit"]
todo :: IO ()
todo = run [] where 
    getSafe :: Int -> [a] -> Maybe a
    getSafe i (_:xs) | i > 0 = getSafe (i-1) xs
    getSafe 0 (x:_) = Just x
    getSafe _ _ = Nothing

    nats :: [Integer]
    nats = [0..]

    run items = do
        putStrLn ""
        choice <- choose "What do you want to do? " actions
        case choice of 
            "add" -> do 
                putStrLn "Enter text of todo item:"
                added <- getLine
                run (added : items)
            "display" -> do
                foldr 
                    (\(l, i) p -> p >> putStrLn (show i ++ ") " ++ l)) 
                    (putStrLn "") --(pure ()) 
                    (reverse (zip (reverse items) nats))
                run items
            "remove" -> do
                putStrLn "Index of item to remove:"
                l <- getLine
                case readMaybe l :: Maybe Int of
                    Nothing -> do 
                        putStrLn "Input is not a number!"
                        run items
                    Just n -> do
                        case getSafe n items of
                            Just item -> run (delete item items)
                            Nothing -> do 
                                putStrLn "Invalid index!"
                                run items
            "quit" -> return ()
            _ -> do -- should be impossible
                putStrLn "ERROR"
                return ()



return []
main :: IO Bool
main = do
    $quickCheckAll