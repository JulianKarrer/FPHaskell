{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where
import Control.Applicative
import Data.Bifunctor (first)
import Data.Char (isAlpha)
import Data.List
import Data.Maybe (fromMaybe)
import GHC.GHCi.Helpers (flushAll)

-- 1.1 PARSER --

newtype Parser t r = Parser { runParser :: [t] -> [(r, [t])] }

-- recognizes the empty language
pempty :: Parser t r
pempty = Parser $ const []

-- recognizes the language with just the empty word
succeed :: r -> Parser t r
succeed r = Parser $ \ts -> [(r, ts)]

-- `satisfy p` recognizes the language { a | p a }
satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
  (t : ts') | p t -> [(t, ts')]
  _               -> []

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


runParserEnd :: Parser t a -> [t] -> [a]
runParserEnd p ts = map fst $ filter (\(_, r) -> null r) $ runParser p ts

instance Functor (Parser t) where
  fmap f pa = Parser $ \ts -> map (first f) (runParser pa ts) -- = pmap

instance Applicative (Parser t) where
  pure = succeed
  (<*>) = pseq

instance Monad (Parser t) where
  pa >>= f = Parser $ \ts ->
    -- run the first parser
    let res_a = runParser pa ts in
    -- take the results and remaining tokens
    let as = map fst res_a in
    let ts' = map snd res_a in
    -- find parser b for each result
    let pbs = map f as in
    -- then run it on the respective remaining tokens
    -- then concatenate the results
    concatMap (uncurry runParser) (zip pbs ts')

instance Alternative (Parser t) where
  empty = pempty
  (<|>) = palt


many0 :: Parser t a -> Parser t [a]
many0 p = pure [] <|> many1 p

many1 :: Parser t a -> Parser t [a]
many1 p = (:) <$> p <*> many0 p

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 p s = pure [] <|> sepBy1 p s

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p s = ((:) <$> p) <*> many0 (s *> p)

alts :: [Char] -> Parser Char Char
alts = foldr (\c p -> p <|> lit c) empty

lits :: String -> Parser Char String
lits = foldr (\ l -> (<*>) ((:) <$> lit l)) (pure "")


-- var ::= [a-zA-Z]+
-- exp ::= var
-- | '(' ws '\' ws var ws '.' ws exp ws ')'
-- | '(' ws exp ws1 exp ws ')'

type Var = String
data Expr =
    EVar Var
    | ELam Var Expr
    | EApp Expr Expr deriving Show

ws :: Parser Char String
ws = many0 $ alts " \n\t\r"
ws1 :: Parser Char String
ws1 = many1 $ alts " \n\t\r"

pVar :: Parser Char Var
pVar = many1 $ satisfy isAlpha

pExp :: Parser Char Expr
pExp =
    EVar <$> pVar <|>
    ELam <$>    (lit '(' *> ws *> lit '\\' *> ws *> pVar <* ws ) <*>
                (lit '.' *> ws *> pExp <* ws <* lit ')') <|>
    EApp <$> (lit '(' *> ws *> pExp ) <*> (ws1 *> pExp <* ws <* lit ')')

parse :: String -> Maybe Expr
parse s = case runParserEnd pExp s of
    [e] -> Just e
    _ -> Nothing

-- 1.2

data Set a = Set {unSet :: [a]}
-- set union
(+*) :: (Eq a) => Set a -> Set a -> Set a
(Set (x:xs)) +* (Set ys) =
    if x `elem` ys then
        Set xs +* Set ys  -- keep elements unique, unlike list
    else
        Set xs +* Set (x:ys)
(Set []) +* (Set ys) = Set ys
-- set minus
(-*) :: (Eq a) => Set a -> Set a -> Set a
(Set xs) -* (Set []) = Set xs
(Set xs) -* (Set (y : ys)) = Set (delete y xs) -* Set ys
-- not set inclusion
notIn :: Eq a => a -> Set a -> Bool
notIn x (Set xs) = x `notElem` xs
-- create singleton set
set :: a -> Set a
set x = Set [x]

free :: Expr -> Set Var
free (EVar x) = set x
free (ELam x e) = free e -* set x
free (EApp e1 e2) = free e1 +* free e2

-- 1.3
freshVars :: [Var]
freshVars = concatMap varsN [1::Integer ..] where
    varsN i | i>1 = [ new ++ prefix | prefix <- varsN (i-1), new <- varsN 1 ]
    varsN _ = fmap (:[]) ['a'..'z'] -- toString

-- ["a","b",..."z","aa".."az"...]
fresh :: Set Var -> Var
fresh vs = fromMaybe "???" (find (`notIn` vs) freshVars)

-- 1.4
-- capture-free substitution
(|->) :: Var -> Expr -> Expr -> Expr
(x |-> n)   (EVar y)   |             x==y = n
(_ |-> _)   (EVar y)                      = EVar y
(x |-> _)   (ELam y m) |             x==y = ELam y m
(x |-> n)   (ELam y m) | y `notIn` free n = ELam y ((x |-> n) m)
(x |-> n)   (ELam y m)                    = let y' = fresh $ free n +* free m in
                                            ELam y' ((x |-> n).(y |-> EVar y') $ m)
(x |-> n)   (EApp m1 m2)                  = EApp ((x |-> n) m1) ((x |-> n) m2)


-- 1.5 
data Step e = Unchanged e | Reduced e

step :: Expr -> Step Expr
-- beta reduction
step (EApp (ELam x m) n) = Reduced $ (x |-> n) m
-- eta reduction
step (ELam x (EApp m (EVar y))) | x==y && x `notIn` free m = Reduced m
-- induction over structure of expressions
step o@(EVar _) = Unchanged o
step o@(ELam x n) = case step n of
    Reduced n' -> Reduced $ ELam x n'
    Unchanged _ -> Unchanged o
step o@(EApp m n) = case step m of
    Reduced m' -> Reduced $ EApp m' n
    Unchanged _ -> case step n of
        Reduced n' -> Reduced $ EApp m n'
        Unchanged _ -> Unchanged o

steps :: Expr -> [Expr]
steps expr = expr: reductions expr where -- prepend the original expression
    reductions e = case step e of
        Unchanged _ -> []
        Reduced e' -> e' : reductions e'


-- 1.7
pretty :: Expr -> String
pretty (EVar x) = x
pretty (ELam x n) = "(λ"++x++". "++pretty n++")"
pretty (EApp n m) = "("++pretty n++" "++pretty m++")"


-- 1.8
main :: IO ()
main = do
    putStr "λ> " >> flushAll
    l <- getLine
    putStrLn ""
    case parse l of
        Nothing -> putStrLn "ERROR: Failed to parse input. Try again!"
        Just e -> foldr ((>>) . putStrLn . pretty) (return ()) (steps e) 
    main

-- ((\x. (x x)) (\x. (x x)))
