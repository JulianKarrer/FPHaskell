module WhileInterp where

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
