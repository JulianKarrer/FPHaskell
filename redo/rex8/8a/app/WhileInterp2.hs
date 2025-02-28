module WhileInterp2 where

-- Variables
type Var = String

-- Values
data Val = VInt Int
         | VBool Bool
         | VUnit
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
          | EPrint Expr
          deriving (Eq, Show)

-- Environments
type Env = [(Var, Val)]

data RuntimeError = UndefinedVar String
                  | TypeError String

-- Semantics of an operator (how to apply an operator to values)
semOp :: Op -> Val -> Val -> Either RuntimeError Val
semOp Add  (VInt x)   (VInt y)   = Right $ VInt (x + y)
semOp Sub  (VInt x)   (VInt y)   = Right $ VInt (x - y)
semOp Less (VInt x)   (VInt y)   = Right $ VBool (x < y)
semOp _    _          _          = Left $ TypeError "Operator cannot handle those arguments"

insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x'   = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env

eval :: Expr -> Env -> IO (Either RuntimeError (Val, Env))
eval (EVar x) env = case lookup x env of
  Just v -> return $ Right (v, env)
  Nothing -> return $ Left $ UndefinedVar x
eval (EVal v) env = return $ Right (v, env)
eval (EOp e1 o e2) env = do
  r1 <- eval e1 env
  case r1 of
    Left err -> return $ Left err
    Right (v1, env') -> do
      r2 <- eval e1 env'
      case r2 of
        Left err -> return $ Left err
        Right (v2, env'') -> do
          case semOp o v1 v2 of
            Left err -> return $ Left err
            Right v -> return $ Right (v, env'')
eval (EAssign x e) env = do
  r <- eval e env
  case r of
    Left err -> return $ Left err
    Right (v, env') -> return $ Right (VUnit, insertOrUpdate x v env')
eval (EWhile e1 e2) env = do
  r1 <- eval e1 env
  case r1 of
    Left err -> return $ Left err
    Right (v1, env') -> do
      case v1 of
        VBool False -> return $ Right (VUnit, env')
        VBool True  -> eval (ESeq e2 (EWhile e1 e2)) env'
        _           -> return $ Left $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) env = do
  r1 <- eval e1 env
  case r1 of
    Left err -> return $ Left err
    Right (v1, env') -> do
      eval e2 env'
eval (EPrint e) env = do
  r <- eval e env
  case r of
    Left err -> return $ Left err
    Right (v, env') -> do
      print v
      return $ Right (VUnit, env')
