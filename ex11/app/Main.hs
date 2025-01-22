{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Text.Read (readMaybe)
import Data.Char (isAlpha, isNumber)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.List (intercalate, nub)
import Data.Foldable (forM_)
import Parser
import Control.Monad.Except
import Control.Monad.State.Strict

-- Syntax ----------------------------------------------------------------------

type Var = String

data Op2 =
    OAdd
  | OLess
  | OAnd
  deriving (Show, Eq)

data Exp =
    EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  | EInt Integer
  | EBool Bool
  | EOp2 Exp Op2 Exp
  deriving (Show, Eq)

-- Parser ----------------------------------------------------------------------

keywords :: [String]
keywords = ["true", "false"]

pVar :: Parser Char Var -- exclude keywords from valid variable names
pVar = many1 (satisfy isAlpha)  >>= (\x -> if x `elem` keywords then empty else return x)

pNum :: Parser Char Integer
pNum = do
  digits <- many1 (satisfy isNumber)
  maybe empty return (readMaybe digits)

pOp2 :: Parser Char Op2
pOp2 = do
  op <- lit '+' <|> lit '<' <|> lit '&'
  case op of
    '+' -> return OAdd
    '<' -> return OLess
    '&' -> return OAnd
    _ -> error "undefined binary operator used!"


pBool :: Parser Char Bool
pBool = do
  res <- lits "true" <|> lits "false"
  if res=="true" then return True else return False

pExp :: Parser Char Exp
pExp =
  EVar <$> pVar
  <|> ELam <$ lit '(' <* ws <* lit '\\' <* ws <*> pVar <* ws <* lit '.' <* ws <*> pExp <* ws <* lit ')'
  <|> EApp <$ lit '(' <* ws <*> pExp <* ws1 <*> pExp <* ws <* lit ')'
  <|> EInt <$> pNum
  <|> EBool <$> pBool
  <|> EOp2 <$ lit '(' <* ws <*> pExp <* ws  <*> pOp2 <* ws <*> pExp <* ws <* lit ')'

parseExp :: String -> Either String Exp
parseExp s = case runParserEnd pExp s of
  []  -> Left "Failed parsing input"
  [e] -> Right e
  es  -> Left "Input can be parsed in multiple ways"

example = parseExp "((\\x. x) (\\y. y))"
example2 = parseExp "(x+ 420)"
example3 = parseExp "(true &false)"
example4 = parseExp "(69<420)"

-- Interpreter -----------------------------------------------------------------

remove :: Eq a => a -> [a] -> [a]
remove x []              = []
remove x (y:ys) | x == y =     remove x ys
remove x (y:ys)          = y : remove x ys

free :: Exp -> [Var]
free (EVar x)     = [x]
free (ELam x e)   = remove x $ free e
free (EApp e1 e2) = free e1 ++ free e2
free (EInt n) = []
free (EBool b) = []
free (EOp2 e1 op e2) = free e1 ++ free e2

fresh :: [Exp] -> Var
fresh es = head $ filter (`notElem` fvs) vars where
  fvs = concatMap free es
  vars = [ "x" ++ show i | i <- [1..] ]

(-->) :: Var -> Exp -> Exp -> Exp
(x --> e') (EVar y) | x == y                = e'
                    | otherwise             = EVar y
(x --> e') (ELam y e) | x == y              = ELam y e
                      | y `notElem` free e' = ELam y ((x --> e') e)
                      | otherwise           = let y' = fresh [e, e'] in
                                              ELam y' ((x --> e') ((y --> EVar y') e))
(x --> e') (EApp e1 e2)                     = EApp ((x --> e') e1) ((x --> e') e2)
(x --> e') (EInt n)                         = EInt n
(x --> e') (EBool b)                        = EBool b
(x --> e') (EOp2 e1 op e2)                  = EOp2 ((x --> e') e1) op ((x --> e') e2)

step :: Exp -> Maybe Exp
step (EApp (ELam x e1) e2) = Just $ (x --> e2) e1
step (ELam x (EApp e (EVar y))) | x == y && x `notElem` free e = Just e
step (EVar x) = Nothing
step (ELam x e) = ELam x <$> step e
step (EApp e1 e2) = (\x -> EApp x e2) <$> step e1
                <|> (\x -> EApp e1 x) <$> step e2
-- new primitives:
step (EInt n) = Nothing
step (EBool b) = Nothing
-- new eliminations:
step (EOp2 (EInt x1) OAdd (EInt x2)) = Just $ EInt (x1 + x2)
step (EOp2 (EInt x1) OLess (EInt x2)) = Just $ EBool (x1 < x2)
step (EOp2 (EBool b1) OAnd (EBool b2)) = Just $ EBool (b1 && b2)
-- new congrucency relations: (propagate into subterms of expression)
step (EOp2 e1 op e2) = (\e1' -> EOp2 e1' op e2) <$> step e1
                   <|> (\e2' -> EOp2 e1 op e2') <$> step e2

steps :: Exp -> [Exp]
steps e = case step e of
  Nothing -> [e]
  Just e' -> e : steps e'

prettyOp :: Op2 -> String
prettyOp op = case op of
  OAdd -> "+"
  OLess -> "<"
  OAnd -> "&"

pretty :: Exp -> String
pretty (EVar x)     = x
pretty (ELam x e)   = "(\\" ++ x ++ ". " ++ pretty e ++ ")"
pretty (EApp e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"
pretty (EInt n) = show n
pretty (EBool b) = if b then "true" else "false"
pretty (EOp2 e1 op e2) = "(" ++ pretty e1 ++ " " ++ prettyOp op ++ " " ++ pretty e2 ++ ")"

repl :: IO ()
repl = do
  putStrLn ""
  putStr "lambda> "
  hFlush stdout
  s <- getLine
  case parseExp s of
    Left err -> do
      putStrLn err
      repl
    Right e -> do
      -- type checker output
      putStrLn "Type:"
      case infer e of
        Left err -> do
          putStrLn $ "  ERROR: " ++ err
        Right t -> do
          putStrLn $ "  " ++ tPretty t
      
      -- reductions output
      putStrLn "Reduction:"
      forM_ (steps e) $ \e' -> do
        putStrLn $ "  " ++ pretty e'

      -- has the reduction succeeded?
      if isValue $ last $ steps e then
        putStrLn "Reduced to value."
      else
        putStrLn "Reduction stuck!"

      repl

isValue :: Exp -> Bool
isValue (EInt _) = True
isValue (EBool _) = True
isValue (ELam _ _) = True
isValue _ = False

main :: IO ()
main = repl

-- Type Checking

data Type =
    TInt
  | TBool
  | TFun Type Type
  | TVar Var
  deriving (Eq, Show)

type TypeError = String

type Ctx = [(Var, Type)]

infer :: Exp -> Either TypeError Type
infer expr = run $ do
  (ctx, tret) <- infer' expr
  -- if the typing assumptions are not empty, there are undefined variables
  if not (null ctx) then
    throwError $ undefError ctx
  else
    -- otherwise, if the return type contains a type variable, throw error
    if containsTVar tret then
      throwError $ "Expression has type "++ tPretty tret ++ ".\n         This type is ambigous as it still contains type variables.\n         To support this expression, we would need proper polymorphism in our type system."
    else
      -- otherwise all checks out
      return tret

containsTVar :: Type -> Bool
containsTVar (TVar _) = True
containsTVar (TFun ta tb) =  containsTVar ta || containsTVar tb
containsTVar _ = False


undefError :: Ctx -> String
undefError ctx = intercalate "\n         " $ (\(v,t) -> "Undefined variable "++ v ++ " of type " ++ tPretty t) <$> ctx

freshT :: Int -> Type
freshT i = TVar $ "alpha:"++show i

infer' :: (MonadState Int m, MonadError TypeError m) => Exp -> m (Ctx, Type)
infer' (EVar x) = do
  -- get new type variable for x
  i <- get
  modify (+1)
  return ([(x, freshT i)], freshT i)
infer' (EInt _) = return ([], TInt)
infer' (EBool _) = return ([], TBool)
infer' (ELam x e) = do
  (a, te) <- infer' e
  case lookup x a of
    Just tx -> return (remove (x, tx) a, TFun tx te)
    Nothing -> do
      i <- get
      modify (+1)
      return (a, TFun (freshT i) te)
infer' (EApp m0 m1) = do
  (a0, t0) <- infer' m0
  (a1, t1) <- infer' m1
  i <- get
  modify (+1)
  let alpha = freshT i in do
    s <- mgu $ ctxEqPairs a0 a1 ++ [(t0, TFun t1 alpha)]
    return (nub $ applySub s a0 ++ applySub s a1, subst s alpha)
infer' (EOp2 m0 op m1) = let (t0constr, t1constr, rettype) = op2types op in do
  (a0, t0) <- infer' m0
  (a1, t1) <- infer' m1
  s <- mgu $ ctxEqPairs a0 a1 ++ [(t0, t0constr), (t1, t1constr)]
  return (nub $ applySub s a0 ++ applySub s a1, rettype)

op2types :: Op2 -> (Type, Type, Type)
op2types OAdd = (TInt, TInt, TInt)
op2types OAnd = (TBool, TBool, TBool)
op2types OLess = (TInt, TInt, TBool)


run :: StateT Int (Either TypeError) a -> Either TypeError a
run mx = evalStateT mx (0 :: Int)

-- substitute type variables in Sub
type Sub = [(Var, Type)]
subst :: Sub -> Type -> Type
subst sub TInt = TInt -- primitives
subst sub TBool = TBool -- primitives
subst sub (TFun t1 t2) = TFun (subst sub t1) (subst sub t2) -- propagation, congruency
subst sub (TVar var) = case lookup var sub of -- elimination
  Just t -> t
  Nothing -> TVar var

applySub :: Sub -> Ctx -> Ctx
applySub sub ctx = [(v, subst sub t) | (v, t) <- ctx]

ctxEqPairs :: Ctx -> Ctx -> [(Type, Type)]
ctxEqPairs ctx1 ctx2 = nub $ [(snd a, snd b) | a <- ctx1, b <- ctx2, fst a == fst b]

tPretty :: Type -> String
tPretty TInt = "Int"
tPretty TBool = "Bool"
tPretty (TVar x) = x
tPretty (TFun t1 t2) = "(" ++ tPretty t1 ++ " -> " ++ tPretty t2 ++ ")"

mgu1 :: MonadError TypeError m => Type -> Type -> m Sub
-- one or more type variables involved
mgu1 t1 (TVar x)                  = return [(x, t1)]
mgu1 (TVar x) t2                  = return [(x, t2)]
-- function types
mgu1 (TFun tfrom1 tto1) (TFun tfrom2 tto2) = do mgu [(tfrom1, tfrom2), (tto1, tto2)]
-- one or more basic type
mgu1 t1 t2 = do
  if t1 == t2 then
    return []
  else
    throwError $ "Cannot unify " ++ tPretty t1 ++ " and " ++ tPretty t2 ++ "!"

subst' :: Sub -> (Type, Type) -> (Type, Type)
subst' sub (t1, t2) = (subst sub t1, subst sub t2)

mgu :: MonadError TypeError m => [(Type, Type)] -> m Sub
mgu [] = return []
mgu ((t1, t2) : ts) = do
  sub <- mgu1 t1 t2
  let t2' = subst' sub <$> ts in do
    rest <- mgu t2'
    return $ sub ++ rest



