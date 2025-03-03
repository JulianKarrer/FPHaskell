{-# LANGUAGE FlexibleContexts #-}

module Main where
import Parser
import GHC.Base ( Alternative((<|>), empty) )
import Set
import Data.Maybe
import Data.List
import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldrM)
import GHC.GHCi.Helpers (flushAll)
import Data.Char (isAlpha)

type Var = String
data Op2 = OAdd | OLess | OAnd
    deriving (Show, Eq)
data Exp =
      EVar Var
    | ELam Var Exp
    | EApp Exp Exp
    | EInt Int
    | EBool Bool
    | EOp2 Exp Op2 Exp
    deriving (Show, Eq)

-- 1.1 PARSERS

keywords :: [String]
keywords = ["true", "false"]

pVar :: Parser Char Var
pVar = do
    var <- many1 $ satisfy isAlpha
    if var `elem` keywords then empty else return var

pOp2 :: Parser Char Op2
pOp2= OAdd <$ lit '+' <|> OLess <$ lit '<' <|> OAnd <$ lit '&'

pBool :: Parser Char Bool
pBool = True <$ lits "true" <|>
        False <$ lits "false"

pExp :: Parser Char Exp
pExp =
    EVar <$> pVar <|>
    ELam <$>    (lit '(' *> ws *> lit '\\' *> ws *> pVar <* ws ) <*>
                (lit '.' *> ws *> pExp <* ws <* lit ')') <|>
    EApp <$>    (lit '(' *> ws *> pExp ) <*>
                (ws1 *> pExp <* ws <* lit ')') <|>
    EInt <$> nat <|>
    EBool <$> pBool <|>
    EOp2 <$>    (lit '(' *> ws *> pExp <* ws ) <*>
                pOp2 <*>
                (ws *> pExp <* ws <* lit ')')

parse :: String -> Maybe Exp
parse s = case runParserEnd pExp s of
    [e] -> Just e
    _ -> Nothing

-- 1.2 FREE AND SUBSTITUTION
fresh :: Set Var -> Var
fresh vs = fromMaybe "???" (find (`notIn` vs) freshVars) where
    freshVars :: [Var]
    freshVars = concatMap varsN [1::Integer ..] where
        varsN i | i>1 = [ new ++ prefix | prefix <- varsN (i-1), new <- varsN 1 ]
        varsN _ = fmap (:[]) ['a'..'z']

free :: Exp -> Set Var
free (EVar x) = set x
free (ELam x e) = free e -* set x
free (EApp e1 e2) = free e1 +* free e2
free (EInt _) = emptySet
free (EBool _) = emptySet
free (EOp2 e1 _ e2) = free e1 +* free e2

(\->) :: Var -> Exp -> Exp -> Exp
(x \-> n)   (EVar y)   |             x==y = n
(_ \-> _)   (EVar y)                      = EVar y
(x \-> _)   (ELam y m) |             x==y = ELam y m
(x \-> n)   (ELam y m) | y `notIn` free n = ELam y ((x \-> n) m)
(x \-> n)   (ELam y m)                    = let y' = fresh $ free n +* free m in
                                            ELam y' ((x \-> n).(y \-> EVar y') $ m)
(x \-> n)   (EApp m1 m2)                  = EApp ((x \-> n) m1) ((x \-> n) m2)
(_ \-> _)   (EInt i)                      = EInt i
(_ \-> _)   (EBool b)                     = EBool b
(x \-> n)   (EOp2 e1 op e2)               = EOp2 ((x \-> n) e1) op ((x \-> n) e2)

-- 1.3 STEP FUNCTION AND REDUCTION
data Step e = Unchanged e | Reduced e

step :: Exp -> Step Exp
-- beta reduction
step (EApp (ELam x m) n) = Reduced $ (x \-> n) m
-- delta reduction (reduce constants)
step (EOp2 (EInt x) OAdd (EInt y)) = Reduced $ EInt $ x + y
step (EOp2 (EInt x) OLess (EInt y)) = Reduced $ EBool $ x < y
step (EOp2 (EBool x) OAnd (EBool y)) = Reduced $ EBool $ x && y
-- eta reduction
step (ELam x (EApp m (EVar y))) | x==y && x `notIn` free m = Reduced m
-- induction over structure of expressions
step o@(EVar _) = Unchanged o
step o@(EInt _) = Unchanged o
step o@(EBool _) = Unchanged o
step o@(ELam x n) = case step n of
    Reduced n' -> Reduced $ ELam x n'
    Unchanged _ -> Unchanged o
step o@(EApp m n) = case step m of
    Reduced m' -> Reduced $ EApp m' n
    Unchanged _ -> case step n of
        Reduced n' -> Reduced $ EApp m n'
        Unchanged _ -> Unchanged o
step o@(EOp2 m op n) = case step m of
    Reduced m' -> Reduced $ EOp2 m' op n
    Unchanged _ -> case step n of
        Reduced n' -> Reduced $  EOp2 m op n'
        Unchanged _ -> Unchanged o

steps :: Exp -> [Exp]
steps expr = expr: reductions expr where
    reductions e = case step e of
        Unchanged _ -> []
        Reduced e' -> e' : reductions e'

-- 1.4 PRETTY PRINTING
pretty :: Exp -> String
pretty (EVar x) = x
pretty (ELam x n) = "(λ"++x++". "++pretty n++")"
pretty (EApp n m) = "("++pretty n++" "++pretty m++")"
pretty (EInt i) = show i
pretty (EBool b) = if b then "true" else "false"
pretty (EOp2 e1 op e2) = "("++pretty e1 ++ (
    case op of
        OAdd -> " + "
        OLess -> " < "
        OAnd -> " & "
    ) ++pretty e2++")"

-- Y = (\\f . ((\\x. (f ( x x ))) (\\x. (f (x x)))))
-- M = (\\x . x)
-- N = (Y M)
-- (M N)
-- ((\\x . 42)) ((\\f . ((\\x. (f ( x x ))) (\\x. (f (x x))))) (\\x . 42))))
--  = 42
-- fixed point of (λx.42) is 42

-- EXERCISE 2 - TYPING

data Type =
    TInt |
    TBool |
    TFun Type Type |
    TVar Var
    deriving Eq

data TypeError =
    InfiniteRecursion Var Type |
    NoUnification Type Type |
    Ambiguous Type |
    Undefined Var Type |
    TypeContextJunk Ctx

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TVar v)= v
  show (TFun f a)= "("++ show f ++" -> "++ show a ++")"

instance Show TypeError where
    show (InfiniteRecursion v t) =
          "ERROR: Infinite Recursion in type "++show t++" variable of type " ++ v
    show (NoUnification t1 t2) =
          "ERROR: Cannot unify "++show t1++" and "++show t2++"!"
    show (Ambiguous t) =
          "ERROR: Expression has type "++show t++".\n"
      ++indent++"       This type is ambigous as it still contains type variables.\n"
      ++indent++"       To support this expression, we would need proper polymorphism\n"
      ++indent++"       in our type system."
    show (Undefined v t) =
          "ERROR: Undefined variable "++v++" of type "++show t
    show (TypeContextJunk ctx) =
          "ERROR: Multiple typing assumptions required to resolve types:\n" ++
           concatMap ((++ "\n") . show) ctx



type Ctx = [(Var, Type)]
type Sub = [(Var, Type)]

freshTVar :: MonadState Int m => m Type
freshTVar = get >>= \i -> return $ TVar $ "α:" ++ show i

(|->) :: Sub -> Type -> Type
s |-> (TVar x) = case lookup x s of
    Just t -> t
    Nothing -> TVar x
s |-> (TFun f a) = TFun (s |-> f) (s |-> a)
_ |-> x = x

(|=>) :: Sub -> Ctx -> Ctx
_ |=> [] = []
s |=> ((v,t):ctxx) = (v, s |-> t) : s |=> ctxx


contains :: Type -> Var -> Bool
TInt `contains` _ = False
TBool `contains` _ = False
(TVar v1) `contains` v = v1 == v
(TFun tf ta) `contains` v = tf `contains` v || ta `contains` v

simple :: Type -> Bool
simple TInt = True
simple TBool = True
simple (TFun t1 t2) = simple t1 && simple t2
simple (TVar _) = False

mgu1 :: MonadError TypeError m => Type -> Type -> m Sub
mgu1 t1 t2 | t1 == t2 = return []
mgu1 t (TVar v) = mgu1 (TVar v) t -- use next line
mgu1 (TVar v) t | t `contains` v = do throwError $ InfiniteRecursion v t
mgu1 (TVar v) t = return [(v, t)]
mgu1 (TFun tf1 ta1) (TFun tf2 ta2) = mgu [(tf1, tf2),(ta1, ta2)]
mgu1 t1 t2 = do throwError $ NoUnification t1 t2

mgu :: MonadError TypeError m => [(Type, Type)] -> m Sub
mgu tps = do
    (sub, _) <- foldrM (\(t1, t2) (acc, f) -> do
            s1 <- mgu1 (f t1) (f t2)
            return (s1 =+= acc,  (s1 |->) . f)
        ) ([]::Sub, id) tps
    return sub

(=#=):: Ctx -> Ctx -> [(Type, Type)]
(=#=) xs = mapMaybe (\(v,t1) -> lookup v xs >>= \t2 -> return (t1, t2))
infix 6 =#=

infer :: Exp -> Either TypeError Type
infer expr =  do
    (ctx, typ) <- run $ infer' expr
    if not $ simple typ then throwError $ Ambiguous typ else 
        case ctx of
            [] -> return typ
            -- ctx contains simple type
            [(v,t)] | simple t -> throwError $ Undefined v t
            c -> throwError $ TypeContextJunk c


run :: StateT Int (Either TypeError) a -> Either TypeError a
run mx = evalStateT mx (0 :: Int)

infer' :: (MonadState Int m, MonadError TypeError m) => Exp -> m (Ctx, Type)
infer' (EVar v) = do
    t <- freshTVar
    return ([(v, t)], t)
infer' (EInt _) = return ([], TInt)
infer' (EBool _) = return ([], TBool)
infer' (ELam x e) = do
    (ctxe, te) <- infer' e
    case lookup x ctxe of
        -- we found out in the expression what type x has
        Just tx -> return ([(v,t) | (v,t)<-ctxe, v/=x ], TFun tx te)
        -- found no restriction of x, make up a new type variable
        Nothing -> do
            tx <- freshTVar
            return (ctxe, TFun tx te)
infer' (EApp f a) = do
    (ctxf, tf) <- infer' f
    (ctxa, ta) <- infer' a
    tr <- freshTVar
    s <- mgu ((tf, TFun ta tr) : ctxf =#= ctxa)
    return (s |=> ctxf =+= s |=> ctxa, s |-> tr)
infer' (EOp2 e1 op e2) = do
    (ctx1, t1) <- infer' e1
    (ctx2, t2) <- infer' e2
    let (constraints, tr) = case op of
            OAdd -> ([(t1, TInt), (t2, TInt)], TInt)
            OAnd -> ([(t1, TBool), (t2, TBool)], TBool)
            OLess -> ([(t1, TInt), (t2, TInt)], TBool)
    s <- mgu $ constraints =+= ctx1 =#= ctx2
    return (s |=> ctx1 =+= s |=> ctx2, s |-> tr)


-- weakly binding set union
(=+=) :: (Eq a) => [a] -> [a] -> [a]
xs =+= ys= nub $ xs ++ ys
infix 1 =+=

isValue :: Exp -> Bool
isValue (EInt _) = True
isValue (EBool _) = True
isValue (ELam _ _) = True
isValue _ = False

indent :: String
indent = "  "

main :: IO ()
main = do
    putStr "λ> " >> flushAll
    l <- getLine
    case parse l of
        Nothing -> putStrLn "ERROR: Failed to parse input. Try again!"
        Just e ->  do
            putStrLn "Type:"
            case infer e of
                Left err -> putStrLn $ indent ++ show err
                Right t ->  putStrLn $ indent ++ show t
            putStrLn "Reduction:"
            let es = steps e
            printAllSteps es
            let efinal = last es
            if isValue efinal then putStrLn "Reduced to value." 
            else putStrLn "Reduction stuck!"
    main


printAllSteps :: [Exp] -> IO ()
printAllSteps = foldr ((>>) . putStrLn. (indent++) . pretty) (return ())
