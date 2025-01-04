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

safeLast :: SafeList elem NonEmpty -> elem
safeLast (Cons x r) = case r of
    Nil -> x
    Cons _ _ -> safeLast r

type family FAppend a b where
    FAppend NonEmpty NonEmpty = NonEmpty
    FAppend NonEmpty Empty = NonEmpty
    FAppend Empty NonEmpty = NonEmpty
    FAppend Empty Empty = Empty

safeAppend :: SafeList elem x -> SafeList elem y -> SafeList elem (FAppend x y)
safeAppend Nil Nil = Nil
safeAppend Nil rest@(Cons _ _) = rest
safeAppend rest@(Cons _ _) Nil = rest
safeAppend (Cons x1 r1) rest@(Cons _ _) = Cons x1 (safeAppend r1 rest)


-- EXERCISE 2
data Value a where
    VInt :: Int -> Value Int
    VBool :: Bool -> Value Bool
data Op1 inouttype where
    ONeg :: Op1 Int -- integer negation
    ONot :: Op1 Bool -- boolean not
data Op2 intypes outtype where
  OAdd :: Op2 Int Int -- integer addition
  OSub :: Op2 Int Int-- integer subtraction
  OMul :: Op2 Int Int-- integer multiplication
  OLt  :: Op2 Int Bool-- integer inequality (less than)
  OAnd :: Op2 Bool Bool -- boolean and
  OOr  :: Op2 Bool Bool-- boolean or

data Z = Z
data Stack types where
    SNil :: Stack Z
    SCons :: a -> Stack ts -> Stack (a, ts)

data Instr before after where
    IPush :: Value a -> Instr (Stack ts) (Stack (Value a, ts))
    IPop :: Instr (Stack (a , rest)) (Stack rest)
    INoop :: Instr before before
    IDup :: Instr (Stack (a, rest)) (Stack (a, (a, rest)))
    IDup2 :: Instr (Stack (a, rest)) (Stack (a, (a, (a, rest))))
    IFlip :: Instr (Stack (a, (b, rest))) (Stack (b, (a, rest)))
    IOp1 :: Op1 a -> Instr (Stack (Value a, rest)) (Stack (Value a, rest))
    IOp2 :: Op2 a b -> Instr (Stack (Value a, (Value a, rest))) (Stack (Value b, rest))
    IIf :: Instr (Stack start) (Stack out1) 
        -> Instr (Stack start) (Stack out1) 
        -> Instr (Stack (Value Bool, start)) (Stack out1)
    ISeq :: Instr a b -> Instr b c -> Instr a c

eval :: Instr s1 s2 -> s1 -> s2
eval (IPush val) s = SCons val s
eval IPop (SCons _ rest) =  rest
eval INoop s = s
eval IDup prev@(SCons x rest) = SCons x prev
eval IDup2 prev@(SCons x rest) = SCons x (SCons x prev)
eval IFlip (SCons a (SCons b rest)) = SCons b (SCons a rest)
-- Op1
eval (IOp1 ONeg) (SCons (VInt x) rest) = SCons (VInt $ -x) rest
eval (IOp1 ONot) (SCons (VBool x) rest) = SCons (VBool $ not x) rest
-- Op2
eval (IOp2 OAdd) (SCons (VInt x) (SCons (VInt y) rest)) = SCons (VInt $ x + y) rest
eval (IOp2 OSub) (SCons (VInt x) (SCons (VInt y) rest)) = SCons (VInt $ x - y) rest
eval (IOp2 OMul) (SCons (VInt x) (SCons (VInt y) rest)) = SCons (VInt $ x * y) rest
eval (IOp2 OLt ) (SCons (VInt x) (SCons (VInt y) rest)) = SCons (VBool $ x < y) rest
eval (IOp2 OAnd) (SCons (VBool x) (SCons (VBool y) rest)) = SCons (VBool $ x && y) rest
eval (IOp2 OOr) (SCons (VBool x) (SCons (VBool y) rest)) = SCons (VBool $ x || y) rest
-- Rest
eval (ISeq instr1 instr2) s = eval instr2 (eval instr1 s)
eval (IIf instr1 instr2) (SCons (VBool predicate) rest) =
    if predicate then eval instr1 rest else eval instr2 rest

