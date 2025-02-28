{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Data.Kind (Type)
import Data.Data (Typeable)

main :: IO ()
main = print 161

-------------------------------------------------------------------------------
-- (a) GADT Definition

-- Define a GADT named Expr that represents expressions with the following constructors:

--     IntLit: Takes an Int and produces an Expr Int.
--     BoolLit: Takes a Bool and produces an Expr Bool.
--     Add: Combines two integer expressions to produce an Expr Int.
--     Equal: Compares two integer expressions for equality, resulting in an Expr Bool.

-- Example Skeleton:

-- {-# LANGUAGE GADTs #-}

-- data Expr a where
--     IntLit  :: Int -> Expr Int
--     BoolLit :: Bool -> Expr Bool
--     Add     :: Expr Int -> Expr Int -> Expr Int
--     Equal   :: Expr Int -> Expr Int -> Expr Bool

-- (b) Evaluator Function

-- Implement an evaluator function:

-- eval :: Expr a -> a

-- that pattern matches on each constructor of Expr and returns a value of the correct type.

-- Hint: Use pattern matching to “refine” the type in each case.
-------------------------------------------------------------------------------

-- 1.a
data Expr a where
    EInt  :: Int -> Expr Int
    EBool :: Bool -> Expr Bool
    EAdd  :: Expr Int -> Expr Int -> Expr Int
    EEqual:: Expr x -> Expr x -> Expr Bool

eval :: Expr a -> a
eval (EInt i) = i
eval (EBool b) = b
eval (EAdd (EInt x) (EInt y)) = x+y
eval (EEqual (EInt x) (EInt y)) = x == y
eval (EEqual (EBool x) (EBool y)) = x == y


-------------------------------------------------------------------------------
-- Exam Question: Length‑Indexed Vectors

-- In this exercise, you'll implement a type‑safe vector whose length is encoded in its type using GADTs and type‑level natural numbers.

-- (a) Type‑Level Naturals

-- Define a data type Nat representing natural numbers with two constructors:

--     Z for zero.
--     S n for the successor of a natural number.

-- Example:

-- data Nat = Z | S Nat

-- (b) Length‑Indexed Vector

-- Define a GADT Vec a n where:

--     VNil represents an empty vector (of length Z).
--     VCons prepends an element to a vector, increasing its length by one.

-- Skeleton:

-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeFamilies #-}

-- data Nat = Z | S Nat

-- data Vec a (n :: Nat) where
--     VNil  :: Vec a Z
--     VCons :: a -> Vec a n -> Vec a (S n)

-- (c) Safe Head Function

-- Implement a function:

-- headVec :: Vec a (S n) -> a

-- that safely returns the first element of a non‑empty vector.

-- Hint: Pattern match on VCons.

-- (d) Vector Append with Type‑Level Addition

-- Define a type family Add that adds two type‑level naturals:

-- type family Add (n :: Nat) (m :: Nat) :: Nat where
--     Add Z     m = m
--     Add (S n) m = S (Add n m)

-- Then, implement:

-- append :: Vec a n -> Vec a m -> Vec a (Add n m)

-- that concatenates two vectors.

-------------------------------------------------------------------------------

data Nat = Z | S Nat

data Vec a (n::Nat) where
    VNil :: Vec () Z 
    (:>):: a -> Vec a n -> Vec a (S n)

infixr 5 :>

headVec :: Vec a (S n) -> a
headVec (e :> r) = e

type family Add (x::Nat) (y::Nat) where
    Add Z      y = y
    Add (S x') y = S (Add x' y)


append :: Vec a n -> Vec a m -> Vec a (Add n m)
append VNil (y :> ys) = y :> ys
append (x :> xs) ys = x :> append xs ys


-------------------------------------------------------------------------------
-- Exam Question: Typed Command Interpreter with GADTs

-- You are to design a type‑safe command interpreter using a GADT. Your task is to define a domain‑specific language (DSL) for commands whose result types are encoded in the type of the command. In your design:

--     Define the GADT

--     Create a GADT named Command a with the following constructors (do not reveal implementation details in your answer):
--         A constructor for a command that, when executed, returns the current system time as a String.
--         A constructor for a command that takes a String (an input message) and echoes it back as its result.
--         A constructor for a command that accepts two Int values and returns their sum (an Int).
--         A constructor for a conditional command that takes a Bool along with two subcommands (both of type Command a), and returns a result of type a by choosing one of the subcommands based on the condition.

--     Implement the Interpreter

--     Write an interpreter function with the following signature:

-- runCommand :: Command a -> IO a

-- This function should execute the given command appropriately in the IO monad.

-- Type Safety Requirements

-- Your design must ensure that the type of the result is enforced at compile time. For example, in the conditional command, the two subcommands must have matching result types, and it should be a compile‑time error if they do not.
-------------------------------------------------------------------------------


data Command a where
    GetTime :: Command String
    Echo :: String -> Command String
    Sum :: Int -> Int -> Command Int
    Conditional :: Bool -> Command a -> Command a -> Command a

runCommand :: Command a -> IO a
runCommand GetTime = return "5:3666"
runCommand (Echo s) = print s >> print s >> return s
runCommand (Sum x y) = print (x+y) >> return (x+y)
runCommand (Conditional True x _) = runCommand x
runCommand (Conditional False _ y) = runCommand y


-------------------------------------------------------------------------------
-- Advanced Exam Question: Heterogeneous List (HList) with Type‑Level Operations

-- Your task is to design a strongly‑typed heterogeneous list (HList) that can store values of different types, where the types of the elements are tracked at the type level. Use GADTs, DataKinds, and type families to enforce and manipulate the type‐information. Complete the following parts without revealing implementation details.

-- (a) HList Definition

-- Define an HList type that carries a type‑level list of types (e.g. using DataKinds) indicating the types of its elements. The definition should ensure that an HList’s type fully determines the types (and order) of its contained values.

-- Requirements:

--     Use a GADT to define HList with a type parameter representing a list of types.
--     Use the DataKinds extension to promote lists to the type level.

-- (b) Safe Head and Tail

-- Implement two functions:

--     hHead :: HList (x ': xs) -> x
--     Retrieves the first element of a non‑empty HList.

--     hTail :: HList (x ': xs) -> HList xs
--     Retrieves the tail of a non‑empty HList.

-- Requirements:

--     These functions must be total on non‑empty lists.
--     Their type signatures should reflect the structure of the type‑level list.
-------------------------------------------------------------------------------
data HList (xs::[Type]) where 
    HNil :: HList '[]
    (:|) :: a -> HList (t :ts) -> HList (a : t : ts) 

hHead :: HList (x ': xs) -> x
hHead (x :| xs) = x

hTail :: HList (x ': xs) -> HList xs
hTail (_ :| xs) = xs

-------------------------------------------------------------------------------







