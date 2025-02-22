{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics where

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

