{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where
import Prelude hiding (False, True, Bool)
import GHC.IO.Handle.Internals (readTextDeviceNonBlocking)


-- Primitive Functors ----------------------------------------------------------

infixr 5 :+:
infixr 6 :*:

newtype Const b a = Const b deriving (Show, Eq)

newtype Id a = Id a deriving (Show, Eq)

data (:+:) f g a = Inl (f a) | Inr (g a) deriving (Show, Eq)

data (:*:) f g a = Pair (f a) (g a) deriving (Show, Eq)

instance Functor (Const b) where
  fmap _ (Const x) = Const x

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Pair x y) = Pair (fmap f x) (fmap f y)

-- Functor Fixpoints -----------------------------------------------------------

newtype Fix f = Fix (f (Fix f))

-- Deriving Show does not work for `Fix`, but we can define it ourselves
-- if we enable the `UndecidableInstances` extension.
-- Since the other functors already have derived Show instances, this allows us
-- to debug print all our generic encodings.
instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix fx) = "(Fix (" ++ show fx ++ "))"

type Algebra f a = f a -> a

foldFix :: Functor f => Algebra f a -> (Fix f -> a)
foldFix alg (Fix fx) = alg $ fmap (foldFix alg) fx

-- Generic Representations -----------------------------------------------------

class Functor f => GenRepr a f | a -> f where
  toGen   :: a -> Fix f
  fromGen :: Fix f -> a

----- START -----

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- EXERCISE 1

data Booly = True | False deriving Show
type BoolF = (Const () :+: Const ())

instance GenRepr Booly BoolF where
  toGen True = Fix (Inr (Const ()))
  toGen False = Fix (Inl (Const ()))

  fromGen (Fix (Inr (Const ()))) = True
  fromGen (Fix (Inl (Const ()))) = False

-- lists
type ListF a = (Const () :+: (Const (Fix a) :*: Id))

instance GenRepr a e => GenRepr [a] (ListF e) where
  toGen [] = Fix (Inl (Const ()))
  toGen (x:xs) = Fix (Inr (Pair (Const (toGen x)) (Id (toGen xs))))

  fromGen (Fix (Inl (Const ()))) = []
  fromGen (Fix (Inr (Pair (Const x) (Id xs)))) = fromGen x : fromGen xs

data Tree a = Leaf | Branch (Tree a) a (Tree a)
type TreeF a = Const () :+: (Id :*: Const (Fix a) :*: Id)

instance GenRepr a e => GenRepr (Tree a) (TreeF e) where
  toGen Leaf = Fix (Inl (Const ()))
  toGen (Branch l v r) =
    Fix (Inr (Pair (Id (toGen l)) (Pair (Const (toGen v)) (Id (toGen r)))))

  fromGen (Fix (Inl (Const ()))) = Leaf
  fromGen (Fix (Inr (Pair (Id l) (Pair (Const v) (Id r))))) =
    Branch (fromGen l) (fromGen v) (fromGen r)

-- EXERCISE 2

class Functor f => GenDepth f where
    genDepth :: f Int -> Int

instance GenDepth (Const b) where 
  genDepth :: Const b Int -> Int
  genDepth (Const _) = 0 
instance GenDepth Id where 
  genDepth :: Id Int -> Int
  genDepth (Id i) = i + 1
instance (GenDepth f, GenDepth g) => GenDepth (f :+: g) where 
  genDepth :: (GenDepth f, GenDepth g) => (:+:) f g Int -> Int
  genDepth (Inl x) = genDepth x
  genDepth (Inr y) = genDepth y
instance (GenDepth f, GenDepth g) => GenDepth (f :*: g) where 
  genDepth :: (GenDepth f, GenDepth g) => (:*:) f g Int -> Int
  genDepth (Pair x y) = max (genDepth x) (genDepth y)

depth :: (GenRepr a f, GenDepth f) => a -> Int
depth x = foldFix genDepth $ toGen x

-- EXERCISE 3

data Bit = O | I deriving (Show, Eq)
type Bits = [Bit]

class Functor f => GenToBits f where
    genBits :: f Bits -> Bits

instance GenToBits f => GenToBits (Const (Fix f)) where 
  genBits :: GenToBits f => Const (Fix f) Bits -> Bits
  genBits (Const x) = foldFix genBits x
instance GenToBits (Const ()) where 
  genBits :: Const () Bits -> Bits
  genBits (Const ()) = []
instance GenToBits Id where 
  genBits :: Id Bits -> Bits
  genBits (Id b) = b
instance (GenToBits f, GenToBits g) => GenToBits (f :+: g) where 
  genBits :: (GenToBits f, GenToBits g) => (:+:) f g Bits -> Bits
  genBits (Inl x) = O : genBits x
  genBits (Inr x) = I : genBits x
instance  (GenToBits f, GenToBits g) => GenToBits (f :*: g) where 
  genBits :: (GenToBits f, GenToBits g) => (:*:) f g Bits -> Bits
  genBits (Pair x y) = genBits x ++ genBits y

toBits :: (GenRepr a f, GenToBits f) => a -> Bits
toBits x = foldFix genBits $ toGen x

bitsExample :: Bits
bitsExample = toBits [[False], [False, True]]

-- EXERCISE 4

-- fromBits' :: (GenRepr a f, GenFromBits f) => Bits -> Maybe (a, Bits)
-- fromBits' = undefined --map' (fromGen . Fix) . fromBitsGen

-- fromBits :: (GenRepr a f, GenFromBits f) => Bits -> Maybe a
-- fromBits bs = case fromBits' bs of
--     Just (x, []) -> Just x
--     _ -> Nothing

-- class Functor f => GenFromBits f where
--     fromBitsGen :: GenFromBits g => Bits -> Maybe (f (Fix g), Bits)

-- instance GenFromBits Id where 
--   fromBitsGen bits = Just $ 
 
