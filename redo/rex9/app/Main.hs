{-# LANGUAGE GADTs, TypeFamilies #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- EXERCISE 1

-- 1.1
data Empty
data NonEmpty

data SafeList e a where
    Nil :: SafeList Empty a
    (:-) :: a -> [a] -> SafeList NonEmpty a

safeHead :: SafeList NonEmpty a -> a
safeHead (x:-_) = x

-- 1.2
safeLast :: SafeList NonEmpty a -> a
safeLast (x :- []) = x
safeLast (x :- xs) = last xs

-- 1.3
type family F a b where
    F Empty Empty = Empty
    F _ _         = NonEmpty

safeAppend :: SafeList e1 a -> SafeList e2 a -> SafeList (F e1 e2) a
safeAppend (x :- xs) (y :- ys)  = x :- (xs ++ (y:ys))
safeAppend (x :- xs) Nil        = x :- xs
safeAppend Nil       (x :- xs)  = x :- xs
safeAppend Nil       Nil        = Nil

-- EXERCISE 2

-- Unary Operators
data Op1 i o where
    ONeg :: Op1 Int Int   -- integer negation
    ONot :: Op1 Bool Bool      -- boolean not

-- Binary Operators
data Op2 i1 i2 o where
  OAdd  :: Op2 Int Int Int      -- integer addition
  OSub  :: Op2 Int Int Int      -- integer subtraction
  OMul  :: Op2 Int Int Int      -- integer multiplication
  OLt   :: Op2 Int Int Bool     -- integer inequality (less than)
  OAnd  :: Op2 Bool Bool Bool   -- boolean and
  OOr   :: Op2 Bool Bool Bool   -- boolean or

-- Instructions
data Instr b a where
    IPush:: v -> Instr b (v, b) -- push the value on the top of the stack.
    IPop :: Instr (a,r) r               -- remove the top value of the stack.
    INoop:: Instr b b                   -- does nothing (leaves the stack unchanged).
    IDup :: Instr (x,r) (x,(x,r))       -- duplicates the the top value of stack.
    IDup2:: Instr (x,r) (x,(x,(x,r)))   -- duplicates the top two values of the stack.
    IFlip:: Instr (x,(y,r)) (y,(x,r))   -- swaps the top two values of the stack.
    IOp1 :: Op1 i o -> Instr (i,r) (o,r)-- apply a unary operator to the top of the stack.
    IOp2 :: Op2 i1 i2 o -> Instr (i1,(i2,r)) (o,r) 
            -- apply a binary operator to the top two values of the stack.
    IIf  :: Instr b a -> Instr b a -> Instr (Bool, b) a
            -- if the top value is True then continue with the first instruction
            -- otherwise continue with the second instruction.
    ISeq :: Instr a b -> Instr b c -> Instr a c 
            -- run two instructions in after each other.


data Stack s where
    SNil :: Stack ()
    (:+) :: v -> Stack s -> Stack (v,s)

infixr 0 :+

type Error = String

evalOp1 :: Op1 i o -> i -> o
evalOp1 ONeg x = - x
evalOp1 ONot x = not x

evalOp2 :: Op2 i1 i2 o -> i1 -> i2 -> o
evalOp2 OAdd x y = x + y
evalOp2 OSub x y = x - y
evalOp2 OMul x y = x * y
evalOp2 OLt  x y = x < y
evalOp2 OAnd x y = x && y
evalOp2 OOr  x y = x || y

eval :: Instr b a -> Stack b -> Stack a
eval (IPush v)                   s = v :+ s 
eval IPop                 (_ :+ r) = r
eval INoop                       s = s
eval IDup                 (x :+ r) = x :+ x :+ r
eval IDup2                (x :+ r) = x :+ x :+ x :+ r
eval IFlip           (x :+ y :+ r) = y :+ x :+ r
eval (IOp1 o)             (x :+ r) = evalOp1 o x :+ r
eval (IOp2 o)        (x :+ y :+ r) = evalOp2 o x y :+ r
eval (IIf i1 i2)          (b :+ s) = if b then eval i1 s else eval i2 s
eval (ISeq i1 i2)                s = eval i2 $ eval i1 s


