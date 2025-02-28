{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where
import Data.Maybe (isNothing, fromMaybe)
import Data.List (nub)
import Data.Char (isAlpha, isNumber)
import Control.Monad (when)

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- FUNCTOR LAWS
-- fmap g . fmap h = fmap (g . h)
-- fmap id = id

-- MONAD LAWS
-- mx >>= return = mx
-- return x >>= f = f x
-- (mx >>= g) >>= h = m >>= (\x -> g x >>= h)

-- EXERCISE 1
-- 1.1
newtype ND a = ND [a] deriving Show


instance Functor ND where
  fmap f (ND xs) = ND $ f <$> xs
  -- fmap f (ND []) = ND []
  -- fmap f (ND (x:xs)) = ND (f x : fmap f xs)

instance Applicative ND where
  pure x = ND [x]
  f <*> (ND xs) = ND $ concatMap (\x -> map (\ff -> ff x) (runND f)) xs

instance Monad ND where
  (ND xs) >>= f = ND $ concatMap (runND.f) xs


runND :: ND a -> [a]
runND (ND xs) = xs
choose :: [a] -> ND a
choose = ND
abort :: ND a
abort = ND []

ex1 :: [Int]
ex1 = runND $ do
  x <- choose [1, 2]
  y <- choose [10, 20]
  return $ x + y
ex2 :: [Int]
ex2 = do runND
  $ do
  x <- choose [1..10]
  if even x then
    return x
  else
    abort

flipCoin :: ND Bool
flipCoin = do choose [True,False]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = do
  x <- choose [True,False]
  y <- choose [True,False]
  return (x,y)

type Graph n = [(n, [n])]
type Coloring n c = [(n, c)]

nodes :: Eq n => Graph n -> [n]
nodes g = nub $ map fst g
unColoured :: Eq n => Graph n -> Coloring n c -> [n]
unColoured g clrg = filter (\n -> isNothing (lookup n clrg)) (nodes g)
coloursAvailable :: (Eq n, Eq c) => n -> Graph n -> Coloring n c -> [c] -> [c]
coloursAvailable n g clr = filter (\c ->
    case lookup n g of
      Just nbrs -> Just c `notElem` map (`lookup` clr) nbrs
      Nothing -> True
  )

solve :: (Eq n, Eq c) => Graph n -> [c] -> ND (Coloring n c)
solve g colors = solve' g colors []

solve' :: (Eq n, Eq c) => Graph n -> [c] -> Coloring n c -> ND (Coloring n c)
solve' g colours clr = do
  -- choose a node
  let nodesAvailable = unColoured g clr
  if null nodesAvailable then return clr else do
    -- node <- choose $ unColoured g clr
    -- choose node deterministically -> no sorting + nub of result
    let node = head nodesAvailable
    -- choose a colour
    let avlbCols = coloursAvailable node g clr colours
    if null avlbCols then abort else do
      colour <- choose avlbCols
      solve' g colours ((node, colour):clr)

exGraph :: Graph Int
exGraph =      --  1
  [ (0, [1,2]) -- / \
  , (1, [3,0]) --0   3
  , (2, [3,0]) -- \ /
  , (3, [1,2]) --  2
  ]

exColorings :: [Coloring Int String]
exColorings = runND $ solve exGraph ["red", "blue"]
-- exColorings is
-- [ [(3,"red"), (2,"blue"), (1,"blue"), (0,"red")]
-- , [(3,"blue"), (2,"red"), (1,"red"), (0,"blue")]


-- 1.2 
newtype Partial a = Partial (Maybe a)

runPartial :: Partial a -> Maybe a
runPartial (Partial x) = x

instance Functor Partial where
  fmap f (Partial x) = Partial $ f <$> x
  -- fmap f (Partial x) = Partial $ case x of
  --   Just x' -> Just $ f x'
  --   Nothing -> Nothing

instance Applicative Partial where
  pure x = Partial $ Just x
  (Partial f) <*> (Partial x) = case f of
    Just f' -> Partial $ f' <$> x
    Nothing -> failure

instance Monad Partial where
  (Partial x) >>= f = maybe failure f x

failure :: Partial a
failure = Partial Nothing

(!?) :: [a] -> Int -> Partial a
(x:_) !? 0 = pure x
(_:xs) !? i | i>0 = xs !? (i-1)
_ !? _ = failure

getCell :: [[a]] -> Int -> Int -> Partial a
getCell m x y = do
  row <- m !? y
  row !? x

-- 1.3
newtype Exception e a = Exception (Either e a)

runException :: Exception e a -> Either e a
runException (Exception x) = x

raise :: e -> Exception e a
raise x = Exception $ Left x

withException :: Partial a -> e -> Exception e a
withException arg err = case runPartial arg of
  Just res -> pure res
  Nothing -> raise err

instance Functor (Exception e) where
  fmap f (Exception x) = Exception $ f <$> x

instance Applicative (Exception e) where
  pure x = Exception $ Right x
  (Exception f) <*> x = case f of
    Left e -> raise e
    Right f' -> f' <$> x

instance Monad (Exception e) where
  x >>= f = case runException x of
    Left e -> raise e
    Right x' -> f x'

validatePassword :: String -> Exception String ()
validatePassword pwd = do
  if not (any isNumber pwd) then
    raise "Password should contain numbers!"
  else if not (any isAlpha pwd) then
    raise "Password should contain letters!"
  else when (length pwd < 8) $ raise "Password too short!"

data MatrixError = InvalidRowIndex | InvalidColIndex
getCell' :: [[a]] -> Int -> Int -> Exception MatrixError a
getCell' m x y = do
  row <- withException (m !? y) InvalidRowIndex
  withException (row !? x) InvalidColIndex

-- 1.4
newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State s) = s

instance Functor (State s) where
  fmap f s = State (\cur -> let (a,s') = runState s cur in (f a, s'))

instance Applicative (State s) where
  pure x = State $ \s->(x,s)
  f <*> x = State $
    \cur ->
      let (f',s') = runState f cur -- run f first, then arg
          (a, s'') = runState x s' in
      (f' a, s'')

instance Monad (State s) where
  s >>= f = State $
    \cur ->
        let (a, s') = runState s cur in
        runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- START CODE FROM WhileInterp.hs
-- Variables
type Var = String

-- Values
data Val = VInt Int
         | VBool Bool
         | VUnit
         | VError String
         deriving (Eq, Show)

-- Binary Operators
data Op = Add | Sub | Less
        deriving (Eq, Show)

-- Expressions
data Expr = EVar Var
          | EVal Val
          | EOp Expr Op Expr
          | EAssign Var Expr
          | EWhile Expr Expr
          | ESeq Expr Expr
          deriving (Eq, Show)

-- Environments
type Env = [(Var, Val)]

-- Semantics of an operator (how to apply an operator to values)
semOp :: Op -> Val -> Val -> Val
semOp Add  (VInt x)   (VInt y)   = VInt (x + y)
semOp Sub  (VInt x)   (VInt y)   = VInt (x - y)
semOp Less (VInt x)   (VInt y)   = VBool (x < y)
semOp _    (VError e) _          = VError e
semOp _    _          (VError e) = VError e
semOp _    _          _          = VError "Operator cannot handle those arguments"

insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x'   = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env

-- END CODE FROM WhileInterp.hs

eval :: Expr -> State Env Val
eval (EVar x) = do
  env <- get
  let err = VError $ "Variable '" ++ x ++ "' is not defined."
  return $ fromMaybe err (lookup x env)
eval (EVal v) = return v
eval (EOp e1 o e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ semOp o v1 v2
eval (EAssign x e) = do
  v <- eval e
  modify (insertOrUpdate x v)
  return VUnit
eval (EWhile e1 e2) = do
  c <- eval e1
  case c of
    VBool False -> return VUnit
    VBool True  -> eval (ESeq e2 (EWhile e1 e2))
    VError e    -> return $ VError e
    _           -> return $ VError "While condition was not a bool"
eval (ESeq e1 e2) = eval e1 *> eval e2


example :: Expr
example = ESeq (EAssign "x" $ EVal $ VInt 0)
  (ESeq (EWhile (EOp (EVar "x") Less (EVal $ VInt 10))
  (EAssign "x" $ EOp (EVar "x") Add (EVal $ VInt 1)))
  (EOp (EVar "x") Add (EVal $ VInt 5)))


exampleResult :: (Val, Env)
exampleResult = (runState $ eval example) []

