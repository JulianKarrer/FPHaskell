{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ex13Solution where

import Data.Bifunctor (first)

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

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix fx) = "(Fix (" ++ show fx ++ "))"

type Algebra f a = f a -> a

foldFix :: Functor f => Algebra f a -> (Fix f -> a)
foldFix alg (Fix fx) = alg $ fmap (foldFix alg) fx

-- Generic Representations -----------------------------------------------------

class Functor f => GenRepr a f | a -> f where
  toGen   :: a -> Fix f
  fromGen :: Fix f -> a

-- Bool

type BoolF = Const () :+: Const ()

instance GenRepr Bool BoolF where
  toGen False = Fix $ Inl $ Const ()
  toGen True = Fix $ Inr $ Const ()
  fromGen (Fix (Inl (Const ()))) = False
  fromGen (Fix (Inr (Const ()))) = True

-- Lists

type ListReprF f = Const () :+: Const (Fix f) :*: Id

instance GenRepr a f => GenRepr [a] (ListReprF f) where
  toGen = \case
    [] -> Fix $ Inl $ Const ()
    (x : xs) -> Fix $ Inr $ Pair (Const (toGen x)) (Id (toGen xs))
  fromGen = foldFix $ \case
    Inl (Const ()) -> []
    Inr (Pair (Const x) (Id xs)) -> fromGen x : xs

-- Trees

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Eq, Show)

type TreeReprF f = Const () :+: Id :*: Const (Fix f) :*: Id

instance GenRepr a f => GenRepr (Tree a) (TreeReprF f) where
  toGen = \case
    Leaf -> Fix $ Inl $ Const ()
    (Branch l x r) -> Fix $ Inr $ Pair (Id (toGen l)) (Pair (Const (toGen x)) (Id (toGen r)))
  fromGen = foldFix $ \case
    Inl (Const ()) -> Leaf
    Inr (Pair (Id l) (Pair (Const x) (Id r))) -> Branch l (fromGen x) r

-- Examples

example' :: Fix (ListReprF BoolF)
example' = toGen [True, False, True]

example2 :: Bool
example2 = fromGen (toGen [True, False, True]) == [True, False, True]

exampleTree :: Fix (TreeReprF BoolF)
exampleTree = toGen $ Branch Leaf True $ Branch Leaf False Leaf

-- Recursion Depth -------------------------------------------------------------

class Functor f => GenDepth f where
  genDepth :: f Int -> Int

instance GenDepth (Const b) where
  genDepth (Const _) = 0

instance GenDepth Id where
  genDepth (Id x) = x + 1

instance (GenDepth f, GenDepth g) => GenDepth (f :+: g) where
  genDepth (Inl fx) = genDepth fx
  genDepth (Inr gx) = genDepth gx

instance (GenDepth f, GenDepth g) => GenDepth (f :*: g) where
  genDepth (Pair fx gx) = max (genDepth fx) (genDepth gx)

depth :: (GenRepr a f, GenDepth f) => a -> Int
depth x = foldFix genDepth $ toGen x

exampleDepth1 :: Bool
exampleDepth1 = depth True == 0

exampleDepth2 :: Bool
exampleDepth2 = depth [True, False, True] == 3 

exampleDepth3 :: Bool
exampleDepth3 = depth (Branch Leaf True (Branch Leaf False Leaf)) == 2

exampleDepth4 :: Bool
exampleDepth4 = depth [[True, False], [True]] == 2

-- Binary Serialization --------------------------------------------------------

data Bit = O | I deriving (Show, Eq)
type Bits = [Bit]

class Functor f => GenToBits f where
  toBitsGen :: f Bits -> Bits

instance GenToBits f => GenToBits (Const (Fix f)) where
  toBitsGen (Const x) = foldFix toBitsGen x

instance GenToBits (Const ()) where
  toBitsGen (Const ()) = []

instance GenToBits Id where
  toBitsGen (Id x) = x

instance (GenToBits f, GenToBits g) => GenToBits (f :+: g) where
  toBitsGen (Inl fx) = O : toBitsGen fx
  toBitsGen (Inr gx) = I : toBitsGen gx

instance (GenToBits f, GenToBits g) => GenToBits (f :*: g) where
  toBitsGen (Pair fx gx) = toBitsGen fx ++ toBitsGen gx

toBits :: (GenRepr a f, GenToBits f) => a -> Bits
toBits = foldFix toBitsGen . toGen 

-- Binary Deserialization ------------------------------------------------------

map' :: (a -> b) -> Maybe (a, Bits) -> Maybe (b, Bits)
map' f = fmap (first f)

class Functor f => GenFromBits f where
  fromBitsGen :: GenFromBits g => Bits -> Maybe (f (Fix g), Bits)

instance GenFromBits f => GenFromBits (Const (Fix f)) where
  fromBitsGen bs = map' (Const . Fix) $ fromBitsGen bs

instance GenFromBits (Const ()) where
  fromBitsGen bs = Just (Const (), bs)

instance GenFromBits Id where
  fromBitsGen bs = map' (Id . Fix) $ fromBitsGen bs

instance (GenFromBits f, GenFromBits g) => GenFromBits (f :+: g) where
  fromBitsGen [] = Nothing
  fromBitsGen (O:bs) = map' Inl $ fromBitsGen bs
  fromBitsGen (I:bs) = map' Inr $ fromBitsGen bs

instance (GenFromBits f, GenFromBits g) => GenFromBits (f :*: g) where
  fromBitsGen bs = do
    (fx, bs') <- fromBitsGen bs
    (gx, bs'') <- fromBitsGen bs'
    return (Pair fx gx, bs'')

fromBits' :: (GenRepr a f, GenFromBits f) => Bits -> Maybe (a, Bits)
fromBits' = map' (fromGen . Fix) . fromBitsGen

fromBits :: (GenRepr a f, GenFromBits f) => Bits -> Maybe a
fromBits bs = case fromBits' bs of
  Just (x, []) -> Just x
  _            -> Nothing

-- Serialization Examples ------------------------------------------------------

bexample1 :: Maybe [Bool]
bexample1 = fromBits (toBits [True, True])

bexample2 :: Maybe [[Bool]]
bexample2 = fromBits (toBits [[True, False], [False]])


