{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen, elements, Arbitrary (arbitrary))
import Data.Functor ((<&>))
import Control.Monad (when)
import Data.List (nub)
import Test.QuickCheck.Test (quickCheck)


type Ident = String
data Expr
  = Var Ident Integer -- x^i
  | Con Double
  | Neg Expr -- - e
  | Add Expr Expr -- e1 + e2
  | Rec Expr -- 1 / e
  | Mul Expr Expr -- e1 * e2
  deriving Show

-- EXERCISE 1 - APPLICATIVE INTERPRETER

-- 1.a
newtype EnvReader env a = Reader (env -> Maybe a)

instance Functor (EnvReader env) where
  fmap :: (a -> b) -> EnvReader env a -> EnvReader env b
  fmap f (Reader get) = Reader $ \env -> case get env of
    Just a -> Just $ f a
    Nothing -> Nothing

instance Applicative (EnvReader env) where
  pure :: a -> EnvReader env a
  pure x = Reader $ const $ Just x
  (<*>) :: EnvReader env (a -> b) -> EnvReader env a -> EnvReader env b
  (Reader f) <*> (Reader a) = Reader $ \env -> case f env of
    Nothing -> Nothing
    Just f' -> f' <$> a env

runEnvReader :: EnvReader env a -> env -> Maybe a
runEnvReader (Reader r) = r

-- 1.b
type Env k v = [(k, v)]

getKey :: Eq k => Env k v -> k -> Maybe v
getKey env k = lookup k env

-- 1.c

eval :: Expr -> EnvReader (Env Ident Double) Double
eval (Var name i) = Reader $ fmap (^ i) . lookup name
eval (Con x) = pure x
eval (Neg e) = Reader $ fmap (* (- 1)) . runEnvReader (eval e)
eval (Add e1 e2) = Reader $ \env ->
  let e1' = runEnvReader (eval e1) env in
  let e2' = runEnvReader (eval e2) env in
  ((+) <$> e1') <*> e2'
eval (Rec e) = Reader $ \env -> recMaybe $ runEnvReader (eval e) env where
  recMaybe :: Maybe Double -> Maybe Double
  recMaybe d = case d of
    Nothing -> Nothing
    Just 0.0 -> Nothing
    Just d' ->  Just $ 1.0/d'
eval (Mul e1 e2) = Reader $ \env ->
  let e1' = runEnvReader (eval e1) env in
  let e2' = runEnvReader (eval e2) env in
  ((*) <$> e1') <*> e2'

ex1 :: Maybe Double
ex1 = runEnvReader (eval (Con 4711)) []
ex2 :: Maybe Double
ex2 = runEnvReader (eval (Var "x" 1)) []
ex3 :: Maybe Double
ex3 = runEnvReader (eval (Add (Mul (Con 2) (Var "x" 1)) (Con 1))) [("x", 20.5), ("y", 10)]

-- EXERCISE 2 - EXPRESSION SIMPLIFICATION

simplify :: Expr -> Expr
simplify = simply

simply :: Expr -> Expr
-- Apply the arithmetic simplification laws for addition.
simply (Add x (Con 0)) = x
simply (Add (Con 0) x) = x
-- Apply the arithmetic simplification laws for multiplication.
simply (Mul x (Con 0)) = Con 0
simply (Mul (Con 0) x) = Con 0
simply (Mul x (Con 1)) = x
simply (Mul (Con 1) x) = x

-- Any expression without variables should be simplified to a constant.
simply o@(Var _ _) = o
simply (Con x) = Con x
simply (Neg e) = case simply e of
    Con x' -> Con (-x')
    u -> u
simply (Add x y) = case (simply x, simply y) of
    (Con x', Con y') -> Con (x'+y')
    (o1, o2) -> Add o1 o2
simply (Rec e) = case simply e of
    -- A division by zero should not be executed.
    Con 0.0 -> Rec (Con 0)
    Con x' -> Con (1.0/x')
    u -> u
simply (Mul x y) = case (simply x, simply y) of
    (Con x', Con y') -> Con (x' * y')
    (o1, o2) -> Mul o1 o2

ex4 :: Expr
ex4 = simplify $ Add (Neg (Mul (Con 56) (Neg (Neg (Con (-88)))))) (Con (-77))
ex5 :: Expr
ex5 = simplify $ Add (Neg (Rec (Con 0))) (Mul (Con (-1)) (Con (-77)))
ex6 :: Expr
ex6 = simplify $ Mul (Add (Var "x" 1) (Var "y" 1)) (Add (Var "x" 1) (Var "y" 1))

genNonZero :: Gen Double
genNonZero = do 
  d <- arbitrary
  if d == 0 then genNonZero else return d

genExpr :: Gen Expr
genExpr = do
  con <- arbitrary <&> Con
  dNon0 <- genNonZero <&> Con
  i <- arbitrary
  name <- arbitrary
  e1 <- genExpr 
  e2 <- genExpr
  elements [Var name i, con, Neg e1, Add e1 e2, Rec dNon0, Mul e1 e2]

(+++) :: Eq a => [a] -> [a] -> [a]
a +++ b = nub $ a ++ b

getVars :: Expr -> [String]
getVars (Con _) = []
getVars (Var x _) = [x]
getVars (Neg e) = getVars e
getVars (Rec e) = getVars e
getVars (Add e1 e2) = getVars e1 +++ getVars e2
getVars (Mul e1 e2) = getVars e1 +++ getVars e2

genEnv :: Expr -> Gen (Env Ident Double)
genEnv e = do
  mapM (\v -> arbitrary >>= \d -> return (v, d)) (getVars e)

instance Arbitrary Expr where
  arbitrary = genExpr

main :: IO ()
main = return ()
