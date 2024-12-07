{-# LANGUAGE InstanceSigs, NamedFieldPuns#-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Main where
import Data.Maybe (isNothing, fromMaybe)
import Data.Char (isAlpha, isDigit)
import Parser
import Control.Applicative (Alternative (empty, (<|>), some))
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.List (nub)


-- EXERCISE 1
newtype ND a = ND [a]
instance Functor ND where
    fmap :: (a -> b) -> ND a -> ND b
    fmap f (ND a) = ND (map f a)
instance Applicative ND where
    pure :: a -> ND a
    pure x = ND [x]
    (<*>) :: ND (a -> b) -> ND a -> ND b
    (ND fs) <*> (ND xs) = ND $ concatMap (`map` xs) fs


newtype Partial a = Partial (Maybe a) deriving (Show)
runPartial :: Partial a -> Maybe a
runPartial (Partial x) = x
instance Functor Partial where
    fmap :: (a -> b) -> Partial a -> Partial b
    fmap f x = case runPartial x of
        Just val -> Partial (Just (f val))
        Nothing -> Partial Nothing
instance Applicative Partial where
    pure :: a -> Partial a
    pure = Partial . Just
    (<*>) :: Partial (a -> b) -> Partial a -> Partial b
    (Partial (Just f)) <*> (Partial (Just x)) = pure $ f x
    _ <*> _ = Partial Nothing


newtype Exception e a = Exception (Either e a) deriving (Show)
runException :: Exception e a -> Either e a
runException (Exception either) = either
instance Functor (Exception e) where
  fmap :: (a -> b) -> Exception e a -> Exception e b
  fmap f (Exception e) = case e of
    Left e -> Exception (Left e)
    Right a -> Exception (Right $ f a)
instance Applicative (Exception e) where
  pure :: a -> Exception e a
  pure x = Exception $ Right x
  (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (Exception (Right f)) <*> (Exception (Right x)) = Exception . Right $ f x
  (Exception _) <*> (Exception (Left ex)) = Exception . Left $ ex
  (Exception (Left ex)) <*> (Exception _) = Exception . Left $ ex


newtype State s a = State (s -> (a, s))
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State ((\(a, s) -> (f a, s)) . g)
instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (x,s)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    cf <*> a = State $ \s ->
        let (valA, s') = runState a s in
        let (f, s'') = runState cf s' in
        (f valA, s'')
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State g >>= f = State $ \s ->
    let (res, newState) = g s in
    --     (State stateAfterF) = f res
    -- in stateAfterF newState
        runState (f res) newState
runState :: State s a -> s -> (a, s)
runState (State s) = s

-- EXERCISE 2
-- newtype Parser t r = Parser { runParser :: [t] -> [(r, [t])] }
-- 1
runParserEnd :: Eq a => Parser t a -> [t] -> [a]
runParserEnd Parser {runParser} ts = nub $ map fst $ filter (null.snd) (runParser ts)

-- 2
instance Functor (Parser t) where
  fmap :: (a -> b) -> Parser t a -> Parser t b
  fmap = pmap
instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure = succeed
  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  (<*>) = pseq
instance Alternative (Parser t) where
  empty :: Parser t a
  empty = pempty
  (<|>) :: Parser t a -> Parser t a -> Parser t a
  (<|>) = palt
instance Monad (Parser t) where
  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  (Parser pa) >>= f = Parser $ \ts ->
    let results = pa ts in
    concatMap (\(a,ts2) -> runParser (f a) ts2) results


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
        n <- nat
        return $ ENat n
    eadd = do
        lit '('
        e1 <- expr1
        lit '+'
        e2 <- expr1
        lit ')'
        return $ EAdd e1 e2

-- 3
option :: Parser t a -> Parser t (Maybe a)
option p = (Just <$> p) <|> pure Nothing

many1 :: Parser t a -> Parser t [a]
many1 pa = do
    x <- pa
    xs <- many0 pa
    return (x:xs)

many0 :: Parser t a -> Parser t [a]
many0 pa = many1 pa <|> pure []

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 p psep = sepBy1 p psep <|> pure []

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p psep = oneandmore <|> one where
  one = do
    x <- p
    return [x]
  oneandmore = do
    x <- p
    xs <- suffix
    return (x:xs)
  suffix = do
    sep <- psep
    x <- p
    xs <- suffix <|> pure []
    return (x:xs)


-- 4
-- int ::= ’-’? [’0’ − ’9’]+
combine :: (a -> b -> c) -> Parser t a -> Parser t b -> Parser t c
combine f pa pb = do
  xa <- pa
  f xa <$> pb

minus :: Parser Char Int
minus = (\res -> if isNothing res then 1 else -1) <$> option (lit '-')
int :: Parser Char JSON
int = JInt <$> combine (*) minus nat

-- bool ::= ’true’ | ’false’
word :: String -> Parser Char ()
word = foldr (\c p -> lit c *> p) (pure ())

bool :: Parser Char JSON
bool = JBool <$> true <|> JBool <$> false where
  true = True <$ word "true"
  false = False <$ word "false"

-- ws ::= (’ ’ | ’\n’ | ’\t’ | ’\r’)∗
ws :: Parser Char ()
ws = void (many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r'))

-- str ::= ’"’ [ˆ ’"’]∗ ’"’
str :: Parser Char String
str = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

-- json ::= int | bool | ’null’ | str | ’[’ ws jsons? ws ’]’ | ’{’ ws items? ws ’}’
jnull :: Parser Char JSON
jnull = JNull <$ word "null"

jsons :: Parser Char [JSON]
jsons = sepBy0 json (ws <* lit ',' <* ws)
jlist :: Parser Char JSON
jlist = JList <$> (lit '[' *> ws *> jsons <* ws <* lit ']')

jitem :: Parser Char (String, JSON)
jitem = combine (,) (str <* ws <* lit ':' <* ws) json
jitems :: Parser Char [(String, JSON)]
jitems = sepBy0 jitem (ws <* lit ',' <* ws)
jobj :: Parser Char JSON
jobj = JObject <$> (lit '{' *> ws *> jitems <* ws <* lit '}')

json :: Parser Char JSON
json = ws *> (int <|> bool <|> jnull <|> (JString <$> str) <|> jlist <|> jobj) <* ws 


-- some verbose tests
main :: IO ()
main = do
  -- check combinators
  print $ runParserEnd (option (lit 'a')) "" == [Nothing]
  print $ runParserEnd (option (lit 'a')) "a" == [Just 'a']
  print $ null (runParserEnd (option (lit 'a')) "aaa")
  print $ runParserEnd (many0 (lit 'a')) "" == [""]
  print $ runParserEnd (many0 (lit 'a')) "a" == ["a"]
  print $ runParserEnd (many0 (lit 'a')) "aaa" == ["aaa"]
  print $ null (runParserEnd (many1 (lit 'a')) "")
  print $ runParserEnd (many1 (lit 'a')) "a" == ["a"]
  print $ runParserEnd (many1 (lit 'a')) "aaa" == ["aaa"]
  print $ runParserEnd (sepBy0 (lit 'a') (lit ',')) "" == [""]
  print $ runParserEnd (sepBy0 (lit 'a') (lit ',')) "a" == ["a"]
  print $ runParserEnd (sepBy0 (lit 'a') (lit ',')) "a,a" == ["aa"]
  print $ runParserEnd (sepBy0 (lit 'a') (lit ',')) "a,a,a" == ["aaa"]
  print $ null (runParserEnd (sepBy0 (lit 'a') (lit ',')) "a,a,a,")
  -- check json non-terminals
  -- int
  print $ runParserEnd int "123" == [JInt 123]
  print $ runParserEnd int "-123" == [JInt (-123)]
  print $ null (runParserEnd int "asdf")
  print $ null (runParserEnd int "")
  -- bool
  print $ runParserEnd bool "true" == [JBool True]
  print $ runParserEnd bool "false" == [JBool False]
  print $ null (runParserEnd bool "asdf")
  print $ null (runParserEnd bool "")
  -- ws
  print $ runParserEnd ws "" == [()]
  print $ null (runParserEnd ws "asdf")
  print $ runParserEnd ws " " == [()]
  print $ runParserEnd ws "\t" == [()]
  print $ runParserEnd ws "\n" == [()]
  print $ runParserEnd ws "\r" == [()]
  -- str
  print $ runParserEnd str "\"asdf\"" == ["asdf"]
  print $ runParserEnd str "\"\"" == [""]
  print $ null (runParserEnd str "\"")
  print $ null (runParserEnd str "\"asdf")
  -- null
  print $ null (runParserEnd jnull "nulllasdf")
  print $ runParserEnd jnull "null" == [JNull]
  -- jlist
  print $ runParserEnd jsons "12 , \"asdf\"" == [[JInt 12, JString "asdf"]]
  print $ runParserEnd jsons "12 , 13" == [[JInt 12, JInt 13]]
  print $ runParserEnd jsons "12 , 13" == [[JInt 12, JInt 13]]
  print $ null (runParserEnd jlist "[ 12 , 14")
  print $ null (runParserEnd jlist "")
  print $ runParserEnd jlist "[12, 13]" == [JList [JInt 12, JInt 13]]
  print $ runParserEnd jlist "[12, 13]" == [JList [JInt 12, JInt 13]]
  print $ runParserEnd jlist "[12]" == [JList [JInt 12]]
  print $ runParserEnd jlist "[]" == [JList []]
  -- jobj
  print $ runParserEnd jobj "{\"name\":13}" == [JObject [("name", JInt 13)]]
  -- json
  do 
    test  <- runParserEnd json <$> readFile "example.json"
    print $ test == [
        JObject [
          ("foo", JInt 42),
          ("bar", JList [JInt 1, JInt 2, JBool False, JNull]),
          ("baz", JString "boo")
        ]
      ]


