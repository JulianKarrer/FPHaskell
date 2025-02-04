{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where
import Generics ( GenRepr(..), Fix(..), type (:+:)(..), Const(..), (:*:) (Pair), Id (..), foldFix )
import Prelude hiding (False, True, Bool)
import Data.Bifunctor ( Bifunctor(first) )

main :: IO ()
main = putStrLn "hello world"

-- EXERCISE 1
-- BOOL
data Bool = False | True deriving (Show)
type BoolF = Const () :+: Const ()
instance GenRepr Bool BoolF  where
 toGen False = Fix $ Inl (Const ())
 toGen True = Fix $ Inr (Const ())
 fromGen (Fix(Inl(Const ()))) = False
 fromGen (Fix(Inr(Const ()))) = True

-- LIST
type ListF e = Const () :+: (Const (Fix e) :*: Id)
instance GenRepr a e => GenRepr [a] (ListF e) where
    toGen [] = Fix $ Inl $ Const ()
    toGen (x:xs) = Fix $ Inr $ Pair (Const $ toGen x) (Id $ toGen xs)
    fromGen (Fix (Inl (Const ()))) = []
    fromGen (Fix (Inr (Pair (Const x) (Id xs)))) = fromGen x : fromGen xs

-- TREE
data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Show)
type TreeF e = Const () :+: (Id :*: (Const (Fix e) :*: Id))
instance GenRepr a e => GenRepr (Tree a) (TreeF e) where
  toGen Leaf = Fix $ Inl $ Const ()
  toGen (Branch t1 n t2) = Fix $ Inr $
    Pair (Id $ toGen t1) (Pair (Const $ toGen n) (Id $ toGen t2))
  fromGen (Fix (Inl (Const ()))) = Leaf
  fromGen (Fix (Inr (Pair (Id t1) (Pair (Const n) (Id t2)))))
    = Branch (fromGen t1) (fromGen n) (fromGen t2)

test1 :: [Bool]
test1 = fromGen $ toGen [True,False, False, True]

-- EXERCISE 2
class Functor f => GenDepth f where
    genDepth :: f Int -> Int

instance GenDepth (Const b) where
    genDepth _ = 0
instance GenDepth Id where
    genDepth (Id x) = 1 + x
instance (GenDepth f, GenDepth g) => GenDepth (f :+: g) where
    genDepth (Inl x) = genDepth x
    genDepth (Inr x) = genDepth x
instance (GenDepth f, GenDepth g) => GenDepth (f :*: g) where
    genDepth (Pair f1 f2) = max (genDepth f1) (genDepth f2)

depth :: (GenRepr a f, GenDepth f) => a -> Int
depth x = foldFix genDepth $ toGen x

-- EXERCISE 3
data Bit = O | I deriving (Show, Eq)
type Bits = [Bit]

class (Functor f) => GenToBits f where
    genBits :: f Bits -> Bits

instance GenToBits f => GenToBits (Const (Fix f)) where
  genBits (Const (Fix f)) = foldFix genBits (Fix f)

instance GenToBits (Const ()) where
    genBits (Const ()) = []

instance (GenToBits f, GenToBits g) => GenToBits (f :+: g) where
    genBits (Inl x) = O : genBits x
    genBits (Inr x) = I : genBits x

instance (GenToBits f, GenToBits g) => GenToBits (f :*: g) where
    genBits (Pair f1 f2) = genBits f1 ++ genBits f2

instance GenToBits Id where
    genBits (Id x) = x

toBits :: (GenRepr a f, GenToBits f) => a -> Bits
toBits x = foldFix genBits $ toGen x

-- EXERCISE 4

class Functor f => GenFromBits f where
    fromBitsGen :: GenFromBits g => Bits -> Maybe (f (Fix g), Bits)

fromBits' :: (GenRepr a f, GenFromBits f) => Bits -> Maybe (a, Bits)
fromBits' = map' (fromGen . Fix) . fromBitsGen
map' :: (Functor f, Bifunctor p) => (a -> b) -> f (p a c) -> f (p b c)
map' f = (first f <$> )
fromBits :: (GenRepr a f, GenFromBits f) => Bits -> Maybe a
fromBits bs = case fromBits' bs of
    Just (x, []) -> Just x
    _ -> Nothing


instance GenFromBits f => GenFromBits (Const (Fix f)) where 
    fromBitsGen bits = do
        (c1,b1) <- fromBitsGen bits
        return (Const (Fix c1), b1)
instance GenFromBits (Const ()) where
    fromBitsGen bits = Just (Const (), bits)
instance GenFromBits Id where 
    fromBitsGen bits = do 
        (c1, b1) <- fromBitsGen bits
        return (Id (Fix c1), b1)
instance (GenFromBits f, GenFromBits g) => GenFromBits (f :+: g) where 
    fromBitsGen (I:bits) = do 
        (c1, b1) <- fromBitsGen bits
        return (Inr c1, b1)
    fromBitsGen (O:bits) = do 
        (c1, b1) <- fromBitsGen bits
        return (Inl c1, b1)
instance (GenFromBits f, GenFromBits g) => GenFromBits (f :*: g) where 
    fromBitsGen bits = do
        (c1, b1) <- fromBitsGen bits
        (c2, b2) <- fromBitsGen b1
        return (Pair c1 c2, b2)

test2 :: Maybe (Tree Bool)
test2 = fromBits $ toBits (Branch (Branch Leaf False (Branch Leaf True Leaf)) True Leaf)

test3 :: Maybe (Tree Bool)
test3 = fromBits [I,I,O,O,I,O,I,O,I,O]













