{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Ex11Solution where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (isAlpha)
import Data.Foldable (forM_)
import Data.List (intercalate)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Text.Read (readMaybe)
import Control.Monad.Identity (Identity(runIdentity))
import Data.Bifunctor (bimap)

-- Parsers from Ex07Solution ---------------------------------------------------

-- Code from lecture with newtype wrapper

newtype Parser t r = Parser { runParser :: [t] -> [(r, [t])] }

-- recognizes the empty language
pempty :: Parser t r
pempty = Parser $ \ts -> []

-- recognizes the language with just the empty word
succeed :: r -> Parser t r
succeed r = Parser $ \ts -> [(r, ts)]

-- `satisfy p` recognizes the language { a | p a }
satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
  (t : ts') | p t -> [(t, ts')]
  _               -> []

-- variation of satisfy
msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy m = Parser $ \ts -> case ts of
  (t: ts) | Just r <- m t -> [(r, ts)]
  _                       -> []

-- `lit t` recognizes { t }
lit :: Eq t => t -> Parser t t
lit t = satisfy (== t)

-- alternative of parsers: recognizes the union of the languages of p1 and p2
palt :: Parser t r -> Parser t r -> Parser t r
palt (Parser p1) (Parser p2) = Parser $ \ts -> p1 ts ++ p2 ts

-- sequence of parsers: recognizes the concatenation of the languages of p1 and p2
pseq :: Parser t (a -> b) -> Parser t a -> Parser t b
pseq (Parser p1) (Parser p2) = Parser $ \ts ->
  [ (f a, ts2) | (f, ts1) <- p1 ts, (a, ts2) <- p2 ts1]

pmap :: (s -> r) -> (Parser t s -> Parser t r)
pmap f (Parser p) = Parser $ \ts -> [ (f s, ts') | (s, ts') <- p ts ]

-- Solution

runParserEnd :: Parser t a -> [t] -> [a]
runParserEnd p ts = [ x | (x, ts') <- runParser p ts, null ts' ]

instance Functor (Parser t) where
  fmap = pmap

instance Applicative (Parser t) where
  pure = succeed
  (<*>) = pseq

instance Alternative (Parser t) where
  empty = pempty
  (<|>) = palt

instance Monad (Parser t) where
  (Parser px) >>= f =
    Parser $ \ts -> px ts >>= \(x, ts') -> runParser (f x) ts'
  
many0 :: Parser t x -> Parser t [x]
many0 p = pure [] <|> many1 p 

many1 :: Parser t x -> Parser t [x]
many1 p = pure (:) <*> p <*> many0 p

poptional :: Parser t r -> Parser t (Maybe r)
poptional p = Just <$> p <|> pure Nothing

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p sep = (:[]) <$> p 
           <|> (:) <$> p <* sep <*> sepBy0 p sep

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 p sep = pure [] <|> sepBy1 p sep

-- Note: `many0`, `many1`, `poptional` are actually automatically implemented
-- as `some`, `many`, and `optional` as this construction works for arbitrary
-- Applicatives with an Alternative instance.
-- See documentation of Alternative: 
--   https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Applicative.html#t:Alternative

lits :: (Eq t) => [t] -> Parser t [t]
lits []     = pure []
lits (t:ts) = pure (:) <*> lit t <*> lits ts

ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

ws1 :: Parser Char [Char]
ws1 = many1 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

string :: Parser Char String
string = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

-- Syntax ----------------------------------------------------------------------

type Var = String

data Op2 = OAdd
         | OLess
         | OAnd
         deriving (Show, Eq)

data Exp = EVar Var 
         | ELam Var Exp 
         | EApp Exp Exp
         | EInt Int
         | EBool Bool
         | EOp2 Exp Op2 Exp
         deriving (Show, Eq)

-- Parser ----------------------------------------------------------------------

pDigit :: Parser Char Int
pDigit = msatisfy (\c -> readMaybe [c])

digitsToInt :: [Int] -> Int
digitsToInt ds = sum $ zipWith (*) ds $ map (10^) $ reverse [0..length ds-1]

pNat :: Parser Char Int
pNat = fmap digitsToInt (many1 pDigit)

pInt :: Parser Char Int
pInt = toInt <$> poptional (lit '-') <*> pNat where
  toInt Nothing  n = n
  toInt (Just _) n = -n

pBool :: Parser Char Bool
pBool = True  <$ lits "true" 
    <|> False <$ lits "false"

pVar :: Parser Char Var
pVar = do
  x <- many1 (satisfy isAlpha)
  if x `elem` ["true", "false"] then
    empty
  else
    return x

pOp2 :: Parser Char Op2
pOp2 = OAdd  <$ lit '+'
   <|> OLess <$ lit '<'
   <|> OAnd  <$ lit '&'

pExp :: Parser Char Exp
pExp = EVar <$> pVar
   <|> ELam <$ lit '(' <* ws <* lit '\\' <* ws <*> pVar <* ws <* lit '.' <* ws <*> pExp <* ws <* lit ')'
   <|> EApp <$ lit '(' <* ws <*> pExp <* ws1 <*> pExp <* ws <* lit ')'
   <|> EInt <$> pInt
   <|> EBool <$> pBool
   <|> EOp2 <$ lit '(' <* ws <*> pExp <* ws <*> pOp2 <* ws <*> pExp <* ws <* lit ')'

parseExp :: String -> Either String Exp
parseExp s = case runParserEnd pExp s of
  []  -> Left "Failed parsing input"
  [e] -> Right e
  es  -> Left "Input can be parsed in multiple ways"

example = parseExp "((\\x. x) (\\y. y))"

-- Interpreter -----------------------------------------------------------------

remove :: Eq a => a -> [a] -> [a]
remove x []              = []
remove x (y:ys) | x == y =     remove x ys
remove x (y:ys)          = y : remove x ys

free :: Exp -> [Var]
free (EVar x)       = [x]
free (ELam x e)     = remove x $ free e
free (EApp e1 e2)   = free e1 ++ free e2
free (EInt _)       = []
free (EBool _)      = []
free (EOp2 e1 _ e2) = free e1 ++ free e2

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
(x --> e') (EInt i)                         = EInt i
(x --> e') (EBool b)                        = EBool b
(x --> e') (EOp2 e1 op e2)                  = EOp2 ((x --> e') e1) op ((x --> e') e2)

step :: Exp -> Maybe Exp
step (EApp (ELam x e1) e2) = Just $ (x --> e2) e1
step (ELam x (EApp e (EVar y))) | x == y && x `notElem` free e = Just e
step (EOp2 (EInt i1) OAdd (EInt i2)) = Just $ EInt (i1 + i2)
step (EOp2 (EInt i1) OLess (EInt i2)) = Just $ EBool (i1 < i2)
step (EOp2 (EBool b1) OAnd (EBool b2)) = Just $ EBool (b1 && b2)
step (EVar x) = Nothing
step (ELam x e) = ELam x <$> step e 
step (EApp e1 e2) = (\x -> EApp x e2) <$> step e1 
                <|> (\x -> EApp e1 x) <$> step e2
step (EInt _) = Nothing
step (EBool _) = Nothing
step (EOp2 e1 op e2) = (\x -> EOp2 x op e2) <$> step e1 
                   <|> (\x -> EOp2 e1 op x) <$> step e2

steps :: Exp -> [Exp]
steps e = case step e of
  Nothing -> [e]
  Just e' -> e : steps e'

isValue :: Exp -> Bool
isValue (ELam _ _)  = True
isValue (EInt _)    = True
isValue (EBool _)   = True
isValue _           = False

pretty :: Exp -> String
pretty (EVar x)     = x
pretty (ELam x e)   = "(\\" ++ x ++ ". " ++ pretty e ++ ")"
pretty (EApp e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"
pretty (EInt i)     = show i
pretty (EBool b)    = show b
pretty (EOp2 e1 op e2) = "(" ++ pretty e1 ++ " " ++ prettyOp2 op ++ " " ++ pretty e2 ++ ")"
  where
    prettyOp2 :: Op2 -> String
    prettyOp2 OAdd  = "+"
    prettyOp2 OLess = "<"
    prettyOp2 OAnd  = "&"

data Type = TInt 
          | TBool 
          | TFun Type Type 
          | TVar Var
          deriving (Eq, Show)

data TypeError = TEMismatchedType Type Type
               | TEUnknownVar Var Type
               | TEAmbigousType Type
               | TERecursiveUnification Var Type
               deriving (Eq, Show)

type MonadInfer m = (MonadState Int m, MonadError TypeError m)

type Ctx = [(Var, Type)]

freshVar :: MonadState Int m => m Var
freshVar = do
  i <- get
  put $ i + 1
  return $ "α:" ++ show i

type TypeSub = [(Var, Type)]

ctxEqs :: Ctx -> Ctx -> [(Type, Type)]
ctxEqs [] _ = []
ctxEqs ((x, t1):c1) c2 =
  case lookup x c2 of
    Nothing -> ctxEqs c1 c2
    Just t2 -> (t1, t2) : ctxEqs c1 c2

mgu1 :: MonadError TypeError m => Type -> Type -> m TypeSub
mgu1 (TVar x)     (TVar y) | x == y           = return []
                           | otherwise        = return [(y, TVar x)]
mgu1 (TVar x)     t        | x `elem` freeT t = throwError $ TERecursiveUnification x t
                           | otherwise        = return [(x, t)]
mgu1 t            (TVar x) | x `elem` freeT t = throwError $ TERecursiveUnification x t
                           | otherwise        = return [(x, t)]
mgu1 TInt         TInt                        = return []
mgu1 TBool        TBool                       = return []
mgu1 (TFun t1 t2) (TFun t1' t2')              = mgu [(t1, t1'), (t2, t2')]
mgu1 t1           t2                          = throwError $ TEMismatchedType t1 t2

mgu :: MonadError TypeError m => [(Type, Type)] -> m TypeSub
mgu [] = return []
mgu ((t1, t2):eqs) = do
  s1 <- mgu1 t1 t2
  s2 <- mgu $ bimap (substType s1) (substType s1) <$> eqs
  return $ s1 ++ s2

substType :: TypeSub -> Type -> Type
substType s TInt = TInt
substType s TBool = TBool
substType s (TFun t1 t2) = TFun (substType s t1) (substType s t2)
substType s (TVar x) = case lookup x s of
  Nothing -> TVar x
  Just t -> t

substCtx :: TypeSub -> Ctx -> Ctx
substCtx s = fmap (\(x, t) -> (x, substType s t))

op2Type :: Op2 -> (Type, Type, Type)
op2Type OAdd  = (TInt, TInt, TInt)
op2Type OLess = (TInt, TInt, TBool)
op2Type OAnd  = (TBool, TBool, TBool)

infer :: MonadInfer m => Exp -> m (Ctx, Type)
infer (EVar x) = do
  alpha <- freshVar
  return ([(x, TVar alpha)], TVar alpha)
infer (ELam x e) = do
  (c, t) <- infer e
  case lookup x c of
    Nothing -> do
      alpha <- freshVar
      return (c, TFun (TVar alpha) t)
    Just tx -> do
      let c' = filter (\(y, t) -> y /= x) c
      return (c', TFun tx t)
infer (EApp e1 e2) = do
  (c1, t1) <- infer e1
  (c2, t2) <- infer e2
  alpha <- freshVar
  s <- mgu((t1, TFun t2 (TVar alpha)) : ctxEqs c1 c2)
  return (substCtx s c1 ++ substCtx s c2, substType s (TVar alpha))
infer (EInt _) = return ([], TInt)
infer (EBool _) = return ([], TBool)
infer (EOp2 e1 op e2) = do
  let (t1', t2', t') = op2Type op
  (c1, t1) <- infer e1
  (c2, t2) <- infer e2
  s <- mgu [(t1, t1'), (t2, t2')]
  return (substCtx s c1 ++ substCtx s c2, t')

freeT :: Type -> [Var]
freeT TInt = []
freeT TBool = []
freeT (TVar x) = [x]
freeT (TFun t1 t2) = freeT t1 ++ freeT t2

run :: StateT Int (Either TypeError) a -> Either TypeError a
run mx = evalStateT mx (0 :: Int)

infer' :: Exp -> Either TypeError Type
infer' e =
  case run (infer e) of
    Left err -> Left err
    Right (ctx, t)
      | not $ null ctx ->
          let (x, t) = head ctx in
          throwError $ TEUnknownVar x t
      | not $ null $ freeT t ->
          throwError $ TEAmbigousType t
      | otherwise -> 
          Right t

prettyType :: Type -> String
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType (TFun t1 t2) = "(" ++ prettyType t1 ++ " -> " ++ prettyType t2 ++ ")"
prettyType (TVar x) = x

repl :: IO ()
repl = do
  putStrLn ""
  putStr "lambda> "
  hFlush stdout
  s <- getLine
  putStrLn ""
  case parseExp s of
    Left err -> do
      putStrLn err
      repl
    Right e -> do
      putStrLn "Type:"
      case infer' e of
        Left err ->
          putStrLn $ "  TYPE ERROR: " ++ show err
        Right t ->
          putStrLn $ "  " ++ prettyType t
      putStrLn "Reduction:"
      forM_ (steps e) $ \e' -> do
        putStrLn $ "  " ++ pretty e'
      if isValue $ last $ steps e then
        putStrLn "Reduced to value."
      else
        putStrLn "ERROR: Reduction got stuck!"
      repl

