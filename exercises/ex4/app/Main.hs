{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.Maybe (isNothing)
import Data.List (intercalate)
import Text.Read (readMaybe)
main :: IO ()
main = putStrLn "Hello, Haskell!"


-- EXERCISE 1
-- 1.1

class Semigroup' a where
    (<+>) :: a -> a -> a
class Semigroup' a => Monoid' a where
    mempty' :: a

-- Bool
instance Semigroup' Bool where
    (<+>) :: Bool -> Bool -> Bool
    (<+>)= (||) 
instance Monoid' Bool where
  mempty' :: Bool
  mempty' = False

-- Int 
instance Semigroup' Int where
    (<+>) :: Int -> Int -> Int
    (<+>) = (+)
instance Monoid' Int where
  mempty' :: Int
  mempty' = 0

-- [a]
instance Semigroup' [a] where
    (<+>) :: [a] -> [a] -> [a]
    (<+>) = (++)
instance Monoid' [a] where
  mempty' :: [a]
  mempty' = []

-- Maybe a
instance Semigroup' (Maybe a) where
    (<+>) :: Maybe a -> Maybe a -> Maybe a
    -- selects the leftmost non-Nothing argument and Nothing otherwise
    (<+>) x y | isNothing y = x
              | otherwise = y
instance Monoid' (Maybe a) where
  mempty' :: Maybe a
  mempty' = Nothing

-- (a,b)
instance (Monoid' a, Monoid' b) => Semigroup' (a,b) where
  (<+>) :: (Monoid' a, Monoid' b) => (a, b) -> (a, b) -> (a, b)
  (<+>) (a, b) (x, y) = (a <+> x, b <+> y)

instance (Monoid' a, Monoid' b) => Monoid' (a,b) where
    mempty' :: (Monoid' a, Monoid' b) => (a, b)
    mempty' = (mempty', mempty')

-- a -> b
instance (Monoid' b) => Semigroup' (a -> b) where
  (<+>) :: Monoid' b => (a -> b) -> (a -> b) -> a -> b
  (<+>) f g x = f x <+> g x

instance (Monoid' b) => Monoid' (a -> b) where
    mempty' :: Monoid' b => a -> b
    mempty' = const mempty'

-- 1.2
mconcat' :: Monoid' m => [m] -> m
mconcat' = foldr (<+>) mempty'

-- 1.3
class Foldable' f where
    foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' :: (a -> b -> b) -> b -> [a] -> b
  foldr' = foldr

instance Foldable' Maybe where
  foldr' :: (a -> b -> b) -> b -> Maybe a -> b
  foldr' f rest x  = case x of
        Nothing -> rest
        Just x' -> f x' rest

data BTree a = Leaf a | Branch (BTree a) (BTree a)

-- instance Foldable' BTree where  
--     foldr' :: (a -> b -> b) -> b -> BTree a -> b
--     foldr' f acc (Leaf x) = f x acc
--     foldr' f acc (Branch l r) = f x acc

instance Foldable' BTree where
  foldr' :: (a -> b -> b) -> b -> BTree a -> b
  foldr' f z = foldr f z . flatten where
    flatten :: BTree a -> [a]
    flatten (Leaf a) = [a]
    flatten (Branch l r) = flatten l ++ flatten r


-- 1.4
fold' :: (Foldable' f, Monoid' a) => f a -> a
fold' = foldr' (<+>) mempty'

-- 1.5
-- `newtype` performance benefit over `data`: 
--    only exists in typechecking, no runtime cost

-- Bool &&
newtype All = All { unAll :: Bool }
instance Semigroup' All where
  (<+>) :: All -> All -> All
  (<+>) (All x) (All y) = All (x && y)
instance Monoid' All where
  mempty' :: All
  mempty' = All True

-- Bool ||
newtype Any = Any { unAny :: Bool }
instance Semigroup' Any where
  (<+>) :: Any -> Any -> Any
  (<+>) (Any x) (Any y) = Any (x || y)
instance Monoid' Any where
  mempty' :: Any
  mempty' = Any False

-- Int +
newtype Sum a = Sum { unSum :: a }
instance Num a => Semigroup' (Sum a) where
  (<+>) :: Sum a-> Sum a-> Sum a
  (<+>) (Sum x) (Sum y) = Sum (x + y)
instance Num a => Monoid' (Sum a) where
  mempty' :: Sum a
  mempty' = Sum 0

-- Int *
newtype Prod a= Prod { unProd :: a }
instance Num a => Semigroup' (Prod a) where
  (<+>) :: Prod a -> Prod a -> Prod a
  (<+>) (Prod x) (Prod y) = Prod (x * y)
instance Num a => Monoid' (Prod a) where
  mempty' :: Prod a
  mempty' = Prod 1


-- EXERCISE 2
-- 2.1a
echoLine :: IO ()
echoLine  = getLine >>= \l -> putStrLn ("Hello, " ++ l ++ "!" )

-- 2.1b
greet :: IO ()
greet =
    putStrLn "What's your name" >>
    getLine >>= \l -> putStrLn ("Hello, " ++ l ++ "!")

-- 2.1c
greetFormal :: IO ()
greetFormal =
    putStrLn "What's your first name?" >>
    getLine >>= \firstName -> 
    putStrLn "What's your surname?" >>
    getLine >>= \surName -> 
    let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!" in
      putStrLn greeting
    

--2.1d
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
    putStrLn (question ++ " [" ++ intercalate "|" answers ++ "]")
    l <- getLine
    if l `elem` answers then
      return l
    else do
      putStrLn "Invalid input. Try again!"
      choose question answers

-- 2.3
enum :: [b] -> [(Int, b)]
enum = zip [0..]

run :: [String] -> IO ()
run todolist = do
    selection <- choose
      "What do you want to do?"
      ["add", "display", "remove", "quit"]
    case selection of
      "add" -> do
        putStrLn "Enter text of todo item:"
        newitem <- getLine
        run $ todolist ++ [newitem]
      "display" -> do
        foldr
          (\(index, item) io -> io >> putStrLn (show index ++ ") " ++ item))
          (return ())
          (reverse $ enum todolist)
        run todolist
      "remove" -> do
        putStrLn "Index of item to remove:"
        remline <- getLine
        case readMaybe remline of
          Nothing -> putStrLn "Invalid index" >> run todolist
          Just rid -> do
            if 0 <= rid && rid < length todolist then do
              putStrLn ("Removed item: " ++ todolist !! rid)
              run (map snd $ filter (\(i, _) -> i /= rid) (enum todolist))
            else 
              putStrLn "Index out of Bounds" >> run todolist
      "quit" -> return ()
      _ -> return () -- impossible case if choose is implemented correctly

todo :: IO ()
todo = run []