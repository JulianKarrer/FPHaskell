module Ex06Solution where

import Data.Char (isDigit, isAlpha)

-- Non-Determinism -------------------------------------------------------------

-- Implementation

newtype ND a = ND [a]

runND :: ND a -> [a]
runND (ND xs) = xs

choose :: [a] -> ND a
choose = ND

abort :: ND a
abort = choose []

instance Functor ND where
  fmap f (ND xs) = ND (map f xs)

instance Applicative ND where
  pure = undefined
  (<*>) = undefined

returnList :: a -> [a]
returnList x = [x]

bindList :: [a] -> (a -> [b]) -> [b]
bindList []       _ = []
bindList (x : xs) f = f x ++ bindList xs f

instance Monad ND where
  return = ND . returnList
  ND mx >>= f = ND $ mx >>= (runND . f)

-- Usage Examples

flipCoin :: ND Bool
flipCoin = choose [False, True]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = do
  coin1 <- flipCoin
  coin2 <- flipCoin
  return (coin1, coin2)

type Graph a = [(a, [a])]

type Coloring a c = [(a, c)]

solve :: (Eq a, Eq c) => Graph a -> [c] -> ND (Coloring a c)
solve g colors = f g [] where
  f [] coloring =
    return coloring
  f ((n, ns) : g') coloring = do
    c <- choose colors
    if any (\n' -> lookup n' coloring == Just c) ns then
      abort
    else
      f g' ((n, c) : coloring)

exGraph :: Graph Int
exGraph =
  [ (0, [1,2])
  , (1, [3,0])
  , (2, [3,0])
  , (3, [1,2])
  ]

exColorings :: [Coloring Int String]
exColorings = runND $ solve exGraph ["red", "blue"]
 
exColoringsPrint :: IO ()
exColoringsPrint = putStrLn $ unlines $ map show exColorings

-- Maybe -----------------------------------------------------------------------

-- Implementation

newtype Partial a = Partial (Maybe a)

runPartial :: Partial a -> Maybe a
runPartial (Partial x) = x

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

instance Functor Partial where
  fmap f (Partial mx) = Partial (mapMaybe f mx)

instance Applicative Partial where
  pure = undefined
  (<*>) = undefined

returnMaybe :: a -> Maybe a
returnMaybe = Just

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x

instance Monad Partial where
  return = Partial . returnMaybe
  Partial mx >>= f = Partial $ bindMaybe mx (runPartial . f)

failure :: Partial a
failure = Partial Nothing

-- Usage Examples

(!?) :: [a] -> Int -> Partial a
[]     !? _ = failure
(x:_)  !? 0 = return x
(_:xs) !? i = xs !? (i - 1)

getCell :: [[a]] -> Int -> Int -> Maybe a
getCell table x y = runPartial $ do
  row <- table !? y 
  col <- row !? x
  return col

getCell' :: [[a]] -> Int -> Int -> Maybe a
getCell' table x y = runPartial $ do
  row <- table !? y 
  row !? x

getCell'' :: [[a]] -> Int -> Int -> Maybe a
getCell'' table x y = runPartial $ table !? y >>= (!? x)

-- Exception -------------------------------------------------------------------

-- Implementation

newtype Exception e a = Exception (Either e a)

runException :: Exception e a -> Either e a
runException (Exception x) = x

mapEither :: (a -> b) -> (Either e a -> Either e b)
mapEither _ (Left e) = Left e
mapEither f (Right x) = Right (f x)

instance Functor (Exception e) where
  fmap f (Exception mx) = Exception (mapEither f mx)

instance Applicative (Exception e) where
  pure = undefined
  (<*>) = undefined

returnEither :: a -> Either e a
returnEither = Right

bindEither :: Either e a -> (a -> Either e b) -> Either e b
bindEither (Left e)  _ = Left e
bindEither (Right x) f = f x

instance Monad (Exception e) where
  return = Exception . returnEither
  Exception mx >>= f = Exception $ bindEither mx (runException . f)

raise :: e -> Exception e a
raise = Exception . Left

withException' :: Maybe a -> e -> Either e a
withException' Nothing e  = Left e
withException' (Just x) _ = Right x

withException :: Partial a -> e -> Exception e a
withException (Partial mx) e = Exception (withException' mx e)

-- Usage Examples

validatePassword :: String -> Exception String ()
validatePassword pw =
  if length pw < 8 then
    raise "Password is less than 8 characters long."
  else if not (any isDigit pw && any isAlpha pw) then
    raise "Password does not contain letters and digits."
  else
    return ()

validatePassword' :: String -> Exception String ()
validatePassword' pw
  | length pw < 8 =
      raise "Password is less than 8 characters long."
  | not $ any isDigit pw && any isAlpha pw =
      raise "Password does not contain letters and digits."
  | otherwise =
      return ()

data TableException = InvalidRowIndex | InvalidColIndex
  deriving (Show, Eq)

getCellE :: [[a]] -> Int -> Int -> Exception TableException a
getCellE table x y = do
  row <- table !? y `withException` InvalidRowIndex
  col <- row   !? x `withException` InvalidColIndex
  return col

-- State -----------------------------------------------------------------------

-- Implementation

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State x) = x

mapFirst :: (a1 -> a2) -> ((a1, b) -> (a2, b))
mapFirst f (x, y) = (f x, y)

instance Functor (State s) where
  fmap f (State g) = State $ mapFirst f . g
  -- Alternative:
  -- fmap f (State g) = State $ \s -> let (x, s') = g s in
  --                                  (f x, s')

instance Applicative (State s) where
  pure = undefined
  (<*>) = undefined

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State mx) >>= f =
    State $ \s -> 
      let (x, s') = mx s in
      runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s ()
set s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  x <- get
  set (f x)

-- Usage Examples

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

eval :: Expr -> Env -> (Val, Env)
eval (EVar x) env = case lookup x env of
  Just v -> (v, env)
  Nothing -> (VError $ "Variable '" ++ x ++ "' is not defined.", env)
eval (EVal v) env = (v, env)
eval (EOp e1 o e2) env =
  let (v1, env') = eval e1 env in
  let (v2, env'') = eval e2 env' in
  (semOp o v1 v2, env'')
eval (EAssign x e) env =
  let (v, env') = eval e env in
  (VUnit, insertOrUpdate x v env')
eval (EWhile e1 e2) env =
  let (v1, env') = eval e1 env in
  case v1 of
    VBool False -> (VUnit, env')
    VBool True  -> eval (ESeq e2 (EWhile e1 e2)) env'
    VError e    -> (VError e, env')
    _           -> (VError "While condition was not a bool", env')
eval (ESeq e1 e2) env =
  let (_, env') = eval e1 env in
  eval e2 env'

eval' :: Expr -> State Env Val
eval' (EVar x) = do
  env <- get
  case lookup x env of
    Just v -> return v
    Nothing -> return $ VError $ "Variable '" ++ x ++ "' is not defined."
eval' (EVal v) = return v
eval' (EOp e1 o e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  return $ semOp o v1 v2
eval' (EAssign x e) = do
  v <- eval' e 
  modify $ insertOrUpdate x v
  return VUnit
eval' (EWhile e1 e2) = do
  v1 <- eval' e1
  case v1 of
    VBool False -> return VUnit
    VBool True  -> eval' $ ESeq e2 (EWhile e1 e2)
    _           -> return $ VError "While condition was not a bool"
eval' (ESeq e1 e2) = do
  _ <- eval' e1
  eval' e2

