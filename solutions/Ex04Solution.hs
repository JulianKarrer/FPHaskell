{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExplicitForAll, TypeApplications, FunctionalDependencies, UndecidableInstances #-}

module Ex04Solution where

import Data.List (isInfixOf, intercalate)
import Text.Read (readMaybe)

-- Semigroups & Monoids --------------------------------------------------------

-- Class Definitions

class Semigroup' a where
  (<+>) :: a -> a -> a
class Semigroup' a => Monoid' a where
  mempty' :: a

-- Instances for Bool

instance Semigroup' Bool where
  (<+>) = (&&)
instance Monoid' Bool where
  mempty' = True

ex1 :: Bool
ex1 = True <+> False <+> True -- => False

-- Instances for Int

instance Semigroup' Int where
  (<+>) = (+)
instance Monoid' Int where
  mempty' = 0

ex2 :: Int
ex2 = 100 <+> 20 <+> 1 -- => 123

-- Instances for Lists

instance Semigroup' [a] where
  (<+>) = (++)
instance Monoid' [a] where
  mempty' = []

ex3 :: [Int]
ex3 = [1, 2] <+> [3] <+> [4, 5] -- => [1, 2, 3, 4, 5]

-- Instances for Maybe

instance Semigroup' (Maybe a) where
  Nothing <+> y = y
  Just x  <+> _ = Just x
instance Monoid' (Maybe a) where
  mempty' = Nothing

ex4 :: Maybe Int
ex4 = Nothing <+> Just 1 <+> Just 2 <+> Nothing -- => Just 1

-- Instances for Pairs

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <+> (x2, y2) = (x1 <+> x2, y1 <+> y2)
instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  mempty' = (mempty', mempty')

ex5 :: (Bool, Int)
ex5 = (True, 100) <+> (False, 20) <+> (True, 1) -- => (False, 123)

ex5' :: (Bool, (Int, Int))
ex5' = (True, (100, 1)) <+> (False, (20, 20)) <+> (True, (1, 300)) -- => (False, (123, 321))

-- Instances for Functions

instance Semigroup' b => Semigroup' (a -> b) where
  (f <+> g) x = f x <+> g x
instance Monoid' b => Monoid' (a -> b) where
  mempty' _ = mempty'

ex6 :: Int -> Bool
ex6 = (>5) <+> (<10) -- the same as \x -> x > 5 && x < 10

ex6' :: Int -> (Int, Bool)
ex6' = f <+> g -- the same as \x -> (x * 2 + x * 3, x > 5 && x < 10)
  where
    f x = (x * 2, x > 5)
    g x = (x * 3, x < 10)

-- mconcat

mconcat' :: Monoid' a => [a] -> a
mconcat' [] = mempty'
mconcat' (x : xs) = x <+> mconcat' xs

mconcat'' :: Monoid' a => [a] -> a
mconcat'' = foldr (<+>) mempty'

-- Foldable --------------------------------------------------------------------

-- Int :: Type
-- Bool :: Type
-- List Int :: Type
-- List :: Type -> Type

class Foldable' t where
  foldr' :: (a -> b -> b) -> b -> t a -> b 
  -- foldr' :: (a -> b -> b) -> b -> [a] -> b 

instance Foldable' [] where
  foldr' = foldr

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x) = [x]

instance Foldable' Maybe where
  foldr' f z = foldr f z . maybeToList'
  -- foldr' _ z Nothing = z
  -- foldr' f z (Just x) = f x z

data BTree a = Leaf a | Branch (BTree a) (BTree a)

flatten :: BTree a -> [a]
flatten (Leaf a) = [a]
flatten (Branch l r) = flatten l ++ flatten r

instance Foldable' BTree where
  foldr' f z = foldr' f z . flatten

-- fold'

fold' :: (Foldable' f, Monoid' m) => f m -> m
fold' = foldr' (<+>) mempty'

example1 :: Int
example1 = fold' [100, 20, 3] -- => 123

example2 :: (Bool, Int)
example2 = fold' [(True, 1), (False, 20), (True, 100)] -- => (False, 121)

example3 :: (Bool, Int)
example3 = fold' $ Just (True, 5) -- => (True, 5)

example4 :: (Bool, Int)
example4 = fold' Nothing -- => (True, 0)

example5 :: (Bool, Int)
example5 = fold' $ Branch (Leaf (True, 1))
                          (Leaf (False, 5)) -- => (False, 6)

-- Newtype Wrappers ------------------------------------------------------------

newtype All a = All { unAll :: a }
newtype Any a = Any { unAny :: a }

instance Semigroup' (All Bool) where
  All x <+> All y = All (x && y)
instance Monoid' (All Bool) where
  mempty' = All True

instance Semigroup' (Any Bool) where
  Any x <+> Any y = Any (x || y)
instance Monoid' (Any Bool) where
  mempty' = Any False

newtype Sum a = Sum { unSum :: a }
newtype Prod a = Prod { unProd :: a }

instance Num a => Semigroup' (Sum a) where
  Sum x <+> Sum y = Sum (x + y)
instance Num a => Monoid' (Sum a) where
  mempty' = Sum 0

instance Num a => Semigroup' (Prod a) where
  Prod x <+> Prod y = Prod (x * y)
instance Num a => Monoid' (Prod a) where
  mempty' = Prod 1

bimap' :: (a1 -> b1) -> (a2 -> b2) -> ((a1, a2) -> (b1, b2))
bimap' f g (x, y) = (f x, g y)


example1' :: Int
example1' = unSum $ fold' $ map Sum [100, 20, 3] -- => 123

example1'alt :: Int
example1'alt = unProd $ fold' $ map Prod [100, 20, 3] -- => 6000

example2' :: (Bool, Int)
example2' = 
  bimap' unAny unSum $
  fold' $
  map (bimap' Any Sum) $
  [(True, 1), (False, 20), (True, 100)] -- => (False, 121)

  -- [(Any True, Sum 1), (Any False, Sum 20), (Any True, Sum 100)] -- => (False, 121)

example3' :: (Bool, Int)
example3' = bimap' unAny unSum $ fold' $ Just (Any True, Sum 5) -- => (True, 5)

-- Newtype & via -------------------------------------------------------------------

-- This was not part of the exercise, but a demonstration of how it is possible to
-- work a bit more nicely with newtype wrappers.
--
-- This section is also the reason why there are so many LANGUAGE pragmas at the top
-- of this file.
--
-- The content of this section is based on the Haskell library `newtype`
-- https://hackage.haskell.org/package/newtype-0.2.2.0/docs/Control-Newtype.html

class Newtype n o | n -> o where
  pack   :: o -> n
  unpack :: n -> o

via :: forall n1 n2 o1 o2. (Newtype n1 o1, Newtype n2 o2) => (n1 -> n2) -> (o1 -> o2)
via f = unpack . f . pack

instance Newtype (All a) a where
  pack = All
  unpack (All x) = x

instance Newtype (Any a) a where
  pack = Any
  unpack (Any x) = x

instance Newtype (Sum a) a where
  pack = Sum
  unpack (Sum x) = x

instance Newtype (Prod a) a where
  pack = Prod
  unpack (Prod x) = x

instance Newtype n o => Newtype [n] [o] where
  pack = map pack
  unpack = map unpack

instance Newtype n o => Newtype (Maybe n) (Maybe o) where
  pack = fmap pack
  unpack = fmap unpack

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (Leaf x) = Leaf (f x)
mapBTree f (Branch l r) = Branch (mapBTree f l) (mapBTree f r)

instance Newtype n o => Newtype (BTree n) (BTree o) where
  pack = mapBTree pack
  unpack = mapBTree unpack

instance (Newtype n1 o1, Newtype n2 o2) => Newtype (n1, n2) (o1, o2) where
  pack = bimap' pack pack
  unpack = bimap' unpack unpack

example1'' :: Int
example1'' = via @[Sum Int] fold' [100, 20, 3] -- => 123

example2'' :: (Bool, Int)
example2'' = via @[(Any Bool, Sum Int)] fold'
               [(True, 1), (False, 20), (True, 100)] -- => (False, 121)

example3'' :: (Bool, Int)
example3'' = via @(Maybe (Any Bool, Sum Int)) fold' $ Just (True, 5) -- => (True, 5)

example4'' :: (Bool, Int)
example4'' = via @(Maybe (Any Bool, Sum Int)) fold' Nothing -- => (True, 0)

example5'' :: (Bool, Int)
example5'' = via @(BTree (Any Bool, Sum Int)) fold' $
               Branch (Leaf (True, 1))
                      (Leaf (False, 5)) -- => (False, 6)

-- IO ------------------------------------------------------------------------------

-- Rewrite without do-notation

echoLine :: IO ()
echoLine = do
  l <- getLine
  putStrLn l

echoLine' :: IO ()
echoLine' = getLine >>= (\l -> putStrLn l)

echoLine'' :: IO ()
echoLine'' = getLine >>= putStrLn

-- (>>) :: Monad m => m a -> m b -> m b
-- ma >> mb = ma >>= (\_ -> mb)

greet :: IO ()
greet = do
  putStrLn "What's your name"
  l <- getLine
  putStrLn ("Hello, " ++ l ++ "!")

greet' :: IO ()
greet' =
  putStrLn "What's your name" >>
  getLine >>= \l ->
  putStrLn ("Hello, " ++ l ++ "!")

greetFormal :: IO ()
greetFormal = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your surname?"
  surName <- getLine
  let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!"
  putStrLn greeting

greetFormal' :: IO ()
greetFormal' =
  putStrLn "What's your first name?" >>
  getLine >>= \firstName ->
  putStrLn "What's your surname?" >>
  getLine >>= \surName ->
  let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!" in
  putStrLn greeting

choices :: IO ()
choices = do
  putStrLn "Do you want to go outside?"
  l <- getLine
  if l == "yes" then do
    putStrLn "It rains!"
    putStrLn "Too, bad!"
  else
    putStrLn "It still rains! Not so bad!"

choices' :: IO ()
choices' =
  putStrLn "Do you want to go outside?" >>
  getLine >>= \l ->
    if l == "yes" then
      putStrLn "It rains!" >>
      putStrLn "Too, bad!"
    else
      putStrLn "It still rains! Not so bad!"

-- Choose

choose :: String -> [String] -> IO String
choose question options = do
  putStrLn $ question ++ " [" ++ intercalate "|" options ++ "]"
  l <- getLine
  if l `elem` options then
    return l
  else do
    putStrLn "Invalid input. Try again!"
    choose question options
  
-- Todo

todo :: IO ()
todo = run [] where
  run :: [String] -> IO ()
  run items = do
    putStrLn ""
    c <- choose "What do you want to do?" ["add", "display", "remove", "quit"]
    case c of
      "add" -> do
        putStrLn "Enter text of todo item:"
        item <- getLine
        run (item : items)
      "display" -> do
        putStrLn ""
        putStrLn $ intercalate "\n"
                 $ map (\(i,s) -> show i ++ ") " ++ s)
                 $ enumerate items
        run items
      "remove" -> do
        putStrLn "Index of item to remove:"
        s <- getLine
        case readMaybe s of
          Nothing -> do
            putStrLn "Invalid index."
            run items
          Just i -> do
            case removeAt i items of
              Nothing -> do
                putStrLn "Invalid index."
                run items
              Just (item, items') -> do
                putStrLn $ "Removed item: " ++ item
                run items'
      "quit" ->
        return ()
      _ -> error "IMPOSSIBLE"

  enumerate :: [a] -> [(Int, a)]
  enumerate = zip [0..]

  removeAt :: Int -> [a] -> Maybe (a, [a])
  removeAt _ [] = Nothing
  removeAt 0 (x : xs) = Just (x, xs)
  removeAt i (x : xs) = case removeAt (i-1) xs of
    Nothing -> Nothing
    Just (y, ys) -> Just (y, x:ys)
    

