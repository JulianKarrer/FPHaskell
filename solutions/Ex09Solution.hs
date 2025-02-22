{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Ex09Solution where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Data.List (intercalate)
import Text.Read (readMaybe)

-- Safe Lists ------------------------------------------------------------------

data Empty
data NonEmpty

data List e a where
  Nil :: List Empty a
  (:-) :: a -> [a] -> List NonEmpty a

infixr 5 :-

data SomeList a = forall e. SomeList (List e a)

head' :: List NonEmpty a -> a
head' (x :- _) = x

tail' :: List NonEmpty a -> [a]
tail' (_ :- xs) = xs

last' :: List NonEmpty a -> a
last' (x :- []) = x
last' (_ :- (x:xs)) = last' (x :- xs)

toList :: List e a -> [a]
toList Nil = []
toList (x :- xs) = x : xs

fromList :: [a] -> SomeList a
fromList [] = SomeList Nil
fromList (x : xs) = SomeList $ x :- xs

fromList' :: [a] -> Either (List Empty a) (List NonEmpty a)
fromList' [] = Left Nil
fromList' (x : xs) = Right $ x :- xs

fromList'' :: [a] -> Maybe (List NonEmpty a)
fromList'' [] = Nothing
fromList'' (x : xs) = Just $ x :- xs

xs :: List NonEmpty Int
xs = 5 :- 3 : 2 : []

type family (:+:) a b where
  Empty    :+: e = e
  NonEmpty :+: _ = NonEmpty

(++-) :: List e1 a -> List e2 a -> List (e1 :+: e2) a
Nil       ++- ys = ys 
(x :- xs) ++- ys = x :- (xs ++ toList ys)

-- class Append e1 e2 e3 | e1 e2 -> e3 where
--   (++-) :: List e1 a -> List e2 a -> List e3 a
--
-- instance Append Empty e2 e2 where
--   _ ++- ys = ys
--
-- instance Append NonEmpty e2 NonEmpty where
--   (x :- xs) ++- Nil = x :- xs
--   (x :- xs) ++- (y :- ys) = x :- (xs ++ [y] ++ ys)

-- Stack Machine ---------------------------------------------------------------

-- Unary Operators
data Op1 a b where
  ONeg :: Op1 Int Int
  ONot :: Op1 Bool Bool

-- Binary Operators
data Op2 a b c where
  OAdd :: Op2 Int Int Int
  OSub :: Op2 Int Int Int
  OMul :: Op2 Int Int Int
  OLt  :: Op2 Int Int Bool
  OAnd :: Op2 Bool Bool Bool
  OOr  :: Op2 Bool Bool Bool

-- Instructions
data Instr s1 s2 where
  IPush :: a -> Instr s (a, s)
  IPop  :: Instr (a, s) s
  INoop :: Instr s s
  IDup  :: Instr (a, s) (a, (a, s))
  IDup2 :: Instr (a, (b, s)) (a, (b, (a, (b, s))))
  IFlip :: Instr (a, (b, s)) (b, (a, s))
  IOp1  :: Op1 a b -> Instr (a, s) (b, s)
  IOp2  :: Op2 a b c -> Instr (a, (b, s)) (c, s)
  IIf   :: Instr s1 s2 -> Instr s1 s2 -> Instr (Bool, s1) s2
  ISeq  :: Instr s1 s2 -> Instr s2 s3 -> Instr s1 s3

data Stack s where
  SNil :: Stack ()
  (:.) :: a -> Stack s -> Stack (a, s)

infixr 5 :.

evalOp1 :: Op1 a b -> a -> b
evalOp1 ONeg = negate
evalOp1 ONot = not

evalOp2 :: Op2 a b c -> a -> b -> c
evalOp2 OAdd = (+)
evalOp2 OSub = (-)
evalOp2 OMul = (*)
evalOp2 OAnd = (&&)
evalOp2 OOr  = (||)

eval :: Instr s1 s2 -> Stack s1 -> Stack s2
eval (IPush v)    s             = v :. s
eval IPop         (_ :. s)      = s
eval INoop        s             = s
eval IDup         (x :. s)      = x :. x :. s
eval IDup2        (x :. y :. s) = x :. y :. x :. y :. s
eval IFlip        (x :. y :. s) = y :. x :. s
eval (IOp1 o)     (x :. s)      = evalOp1 o x :. s
eval (IOp2 o)     (x :. y :. s) = evalOp2 o x y :. s
eval (IIf i1 i2)  (True  :. s)  = eval i1 s
eval (IIf i1 i2)  (False :. s)  = eval i2 s
eval (ISeq i1 i2) s             = eval i2 $ eval i1 s

