{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use optional" #-}
module Main where
import Data.Bifunctor (first)
import Control.Applicative
import Text.Read (readMaybe)

------------------------- PARSER.HS---------------------------

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

-- variation of satisfy
msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy m = Parser $ \ts -> case ts of
  (t: ts') | Just r <- m t -> [(r, ts')]
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

-- JSON Abstract Syntax Tree (AST)

data JSON = JInt Int
          | JBool Bool
          | JNull
          | JString String
          | JList [JSON]
          | JObject [(String, JSON)]
          deriving (Show, Eq)

------------------------END PARSER.HS-------------------------

-- 2.1
runParserEnd :: Parser t a -> [t] -> [a]
runParserEnd p ts = map fst $ filter (\(_, r) -> null r) $ runParser p ts

-- 2.2
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

-- test instances
data Expr1 = ENat Int | EAdd Expr1 Expr1 deriving (Eq, Show)
-- Parsing a single digit
pDigit :: Parser Char Int
pDigit = msatisfy (\c -> readMaybe [c])
-- Converting a list of digits to an integer
digitsToInt :: [Int] -> Int
digitsToInt ds = sum $ zipWith (*) ds $ (10^) <$> reverse [0..length ds-1]
-- Parsing a natural number (we will discuss `some` in the next step)
nat :: Parser Char Int
nat = digitsToInt <$> some pDigit
-- Parsing an Expr1 via the Applicative and Alternative interfaces
expr1 :: Parser Char Expr1
expr1 = ENat <$> nat
    <|> EAdd <$ lit '(' <*> expr1 <* lit '+' <*> expr1 <* lit ')'
-- Alternative: Parsing an Expr1 via the Monad and Alternative interfaces
expr1' :: Parser Char Expr1
expr1' = enat <|> eadd where
    enat = do
        ENat <$> nat
    eadd = do
        _ <- lit '('
        e1 <- expr1
        _ <- lit '+'
        e2 <- expr1
        _ <- lit ')'
        return $ EAdd e1 e2

-- 2.3
-- many1 = do
--     next <- p
--     rest <- many0 p
--     return $ next : rest

option :: Parser t a -> Parser t (Maybe a)
option p = Just <$> p <|> pure Nothing

many0 :: Parser t a -> Parser t [a]
many0 p = pure [] <|> many1 p

many1 :: Parser t a -> Parser t [a]
many1 p = (:) <$> p <*> many0 p

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 p s = pure [] <|> sepBy1 p s

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p s = ((:) <$> p) <*> many0 (s *> p)

-- 2.4

alts :: [Char] -> Parser Char Char
alts = foldr (\c p -> p <|> lit c) empty

lits :: String -> Parser Char String
lits = foldr (\ l -> (<*>) ((:) <$> lit l)) (pure "")

pSign :: Parser Char Int
pSign = maybe 1 (const (-1)) <$> option (lit '-')

int :: Parser Char Int
int = (*) <$> pSign <*> nat

bool :: Parser Char Bool
bool = (True <$ lits "true") <|> (False <$ lits "false")

str :: Parser Char String
str = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

ws :: Parser Char String
ws = many0 $ alts " \n\t\r"

json' :: Parser Char JSON
json' =
    JInt <$> int <|>
    JBool <$> bool  <|>
    JNull <$ lits "null" <|>
    JString <$> str  <|>
    lit '[' *> ws *> (JList <$> jsons)<* ws <* lit ']' <|>
    lit '{' *> ws *> (JObject <$> items)<* ws <* lit '}'


jsons :: Parser Char [JSON]
jsons = sepBy0 json' (ws *> lit ',' *> ws)

item :: Parser Char (String, JSON)
item = (,) <$> str <* ws <* lit ':' <* ws <*> json'

items :: Parser Char [(String, JSON)]
items = sepBy0 item (ws *> lit ',' *> ws)

json :: Parser Char JSON
json = ws *> json' <* ws


main :: IO ()
main = do
    file <- readFile "app/example.json"
    print $ runParserEnd json file

