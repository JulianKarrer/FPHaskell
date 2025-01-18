{-# LANGUAGE GADTs, TypeFamilies #-}
module Main where

main :: IO ()
main = do
    print $ (1::Integer) == safeHead (Cons 1 (Cons 2 Nil))
    print $ safeLast (Cons True Nil)
    print $ safeHead (Cons True Nil)


-- EXERCISE 1
data Empty
data NonEmpty

data SafeList elem empty where
    Nil :: SafeList elem Empty
    Cons :: elem -> SafeList elem doesntmatter -> SafeList elem NonEmpty

safeHead :: SafeList elem NonEmpty -> elem
safeHead (Cons x _) = x

data SomeList a where
    SomeList :: SafeList a e -> SomeList a
safeTail :: SafeList elem NonEmpty -> SomeList elem
safeTail (Cons _ rest) = SomeList rest

safeLast :: SafeList elem NonEmpty -> elem
safeLast (Cons x r) = case r of
    Nil -> x
    Cons _ _ -> safeLast r

type family FAppend a b where
    FAppend NonEmpty _ = NonEmpty
    FAppend Empty e = e

safeAppend :: SafeList elem x -> SafeList elem y -> SafeList elem (FAppend x y)
safeAppend Nil rest = rest
safeAppend rest@(Cons _ _) Nil = rest
safeAppend (Cons x1 r1) rest@(Cons _ _) = Cons x1 (safeAppend r1 rest)


-- EXERCISE 2
data Value a where
    VInt :: Int -> Value Int
    VBool :: Bool -> Value Bool
data Op1 inp out where
    ONeg :: Op1 Int Int -- integer negation
    ONot :: Op1 Bool Bool -- boolean not
data Op2 in1 in2 out where
  OAdd :: Op2 Int Int Int -- integer addition
  OSub :: Op2 Int Int Int-- integer subtraction
  OMul :: Op2 Int Int Int-- integer multiplication
  OLt  :: Op2 Int Int Bool-- integer inequality (less than)
  OAnd :: Op2 Bool Bool Bool -- boolean and
  OOr  :: Op2 Bool Bool Bool-- boolean or

data Z = Z
data Stack types where
    SNil :: Stack Z
    (:+) :: a -> Stack ts -> Stack (a, ts)

data Instr before after where
    IPush :: Value a -> Instr (Stack ts) (Stack (Value a, ts))
    IPop :: Instr (Stack (a , rest)) (Stack rest)
    INoop :: Instr before before
    IDup :: Instr (Stack (a, rest)) (Stack (a, (a, rest)))
    IDup2 :: Instr (Stack (a, rest)) (Stack (a, (a, (a, rest))))
    IFlip :: Instr (Stack (a, (b, rest))) (Stack (b, (a, rest)))
    IOp1 :: Op1 a b -> Instr (Stack (Value a, rest)) (Stack (Value b, rest))
    IOp2 :: Op2 a b c -> Instr (Stack (Value a, (Value b, rest))) (Stack (Value c, rest))
    IIf :: Instr (Stack start) (Stack out1)
        -> Instr (Stack start) (Stack out1)
        -> Instr (Stack (Value Bool, start)) (Stack out1)
    ISeq :: Instr a b -> Instr b c -> Instr a c

eval :: Instr s1 s2 -> s1 -> s2
eval (IPush val) s = val :+ s
eval IPop (_ :+ rest) =  rest
eval INoop s = s
eval IDup prev@(x :+ rest) = x :+ prev
eval IDup2 prev@(x :+ rest) = x :+ (x :+ prev)
eval IFlip (a :+ (b :+ rest)) = b :+ (a :+ rest)
-- Op1
eval (IOp1 ONeg) ((VInt x) :+ rest) = VInt (-x) :+ rest
eval (IOp1 ONot) ((VBool x) :+ rest) = VBool (not x) :+ rest
-- Op2
eval (IOp2 OAdd) ((VInt x) :+ ((VInt y) :+ rest)) = VInt (x + y) :+ rest
eval (IOp2 OSub) ((VInt x) :+ ((VInt y) :+ rest)) = VInt (x - y) :+ rest
eval (IOp2 OMul) ((VInt x) :+ ((VInt y) :+ rest)) = VInt (x * y) :+ rest
eval (IOp2 OLt ) ((VInt x) :+ ((VInt y) :+ rest)) = VBool (x < y) :+ rest
eval (IOp2 OAnd) ((VBool x) :+ ((VBool y) :+ rest)) = VBool (x && y) :+ rest
eval (IOp2 OOr) ((VBool x) :+ ((VBool y) :+ rest)) = VBool (x || y) :+ rest
-- Rest
eval (ISeq instr1 instr2) s = eval instr2 (eval instr1 s)
eval (IIf instr1 instr2) ((VBool predicate) :+ rest) =
    if predicate then eval instr1 rest else eval instr2 rest

