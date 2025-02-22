module Ex10Solution where

import Control.Applicative
import Text.Read (readMaybe)
import Data.Char (isAlpha)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.List (intercalate)
import Data.Foldable (forM_)

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

data Exp = EVar Var 
         | ELam Var Exp 
         | EApp Exp Exp
         deriving (Show, Eq)

-- Parser ----------------------------------------------------------------------

pVar :: Parser Char Var
pVar = many1 (satisfy isAlpha)

pExp :: Parser Char Exp
pExp = EVar <$> pVar
   <|> ELam <$ lit '(' <* ws <* lit '\\' <* ws <*> pVar <* ws <* lit '.' <* ws <*> pExp <* ws <* lit ')'
   <|> EApp <$ lit '(' <* ws <*> pExp <* ws1 <*> pExp <* ws <* lit ')'

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
free (EVar x)     = [x]
free (ELam x e)   = remove x $ free e
free (EApp e1 e2) = free e1 ++ free e2

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

step :: Exp -> Maybe Exp
step (EApp (ELam x e1) e2) = Just $ (x --> e2) e1
step (ELam x (EApp e (EVar y))) | x == y && x `notElem` free e = Just e
step (EVar x) = Nothing
step (ELam x e) = ELam x <$> step e 
step (EApp e1 e2) = (\x -> EApp x e2) <$> step e1 
                <|> (\x -> EApp e1 x) <$> step e2

steps :: Exp -> [Exp]
steps e = case step e of
  Nothing -> [e]
  Just e' -> e : steps e'

pretty :: Exp -> String
pretty (EVar x)     = x
pretty (ELam x e)   = "(\\" ++ x ++ ". " ++ pretty e ++ ")"
pretty (EApp e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"

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
      forM_ (steps e) $ \e' -> do
        putStrLn $ pretty e'
      repl

