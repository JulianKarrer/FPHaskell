module Ex07Solution where
import Control.Applicative
import Text.Read (readMaybe)
import Data.Char (isAlpha)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.List (intercalate)

-- Applicative Instances for Monads --------------------------------------------

-- Non-Determinism

newtype ND a = ND [a]

instance Functor ND where
  fmap f (ND xs) = ND (map f xs)

instance Applicative ND where
  pure x = ND [x] 
  (ND ff) <*> (ND fx) = ND [ f x | f <- ff, x <- fx ]

-- Partiality

newtype Partial a = Partial (Maybe a)

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

instance Functor Partial where
  fmap f (Partial mx) = Partial (mapMaybe f mx)

instance Applicative Partial where
  pure = Partial . Just
  Partial (Just f) <*> Partial (Just x) = Partial $ Just $ f x
  _                <*> _                = Partial Nothing

-- Exceptions

newtype Exception e a = Exception (Either e a)

mapEither :: (a -> b) -> (Either e a -> Either e b)
mapEither _ (Left e) = Left e
mapEither f (Right x) = Right (f x)

instance Functor (Exception e) where
  fmap f (Exception mx) = Exception (mapEither f mx)

instance Applicative (Exception e) where
  pure = Exception . Right
  Exception (Right f) <*> Exception (Right x) = Exception $ Right $ f x
  Exception (Left e)  <*> _                   = Exception $ Left e
  _                   <*> Exception (Left e)  = Exception $ Left e

-- State

newtype State s a = State (s -> (a, s))

mapFirst :: (a1 -> a2) -> ((a1, b) -> (a2, b))
mapFirst f (x, y) = (f x, y)

instance Functor (State s) where
  fmap f (State g) = State $ mapFirst f . g

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State ff <*> State fx = State $ \s1 ->
    let (f, s2) = ff s1 in
    let (x, s3) = fx s2 in
    (f x, s3)

-- Parsers ---------------------------------------------------------------------

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

data Expr1 = ENat Int | EAdd Expr1 Expr1 deriving (Eq, Show)

pDigit :: Parser Char Int
pDigit = msatisfy (\c -> readMaybe [c])

digitsToInt :: [Int] -> Int
digitsToInt ds = sum $ zipWith (*) ds $ map (10^) $ reverse [0..length ds-1]

nat :: Parser Char Int
nat = fmap digitsToInt (many1 pDigit)

expr1 :: Parser Char Expr1
expr1 = enat <|> eadd where
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

expr1' :: Parser Char Expr1
expr1' = ENat <$> nat
     <|> EAdd <$ lit '(' <*> expr1 <* lit '+' <*> expr1 <* lit ')'

example = runParser expr1 "(2 + (3 + 5))"

ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

int :: Parser Char Int
int = toInt <$> poptional (lit '-') <*> nat where
  toInt Nothing  n = n
  toInt (Just _) n = -n

bool :: Parser Char Bool
bool = True  <$ lits "true" 
   <|> False <$ lits "false"

data JSON = JInt Int
          | JBool Bool
          | JNull
          | JString String
          | JList [JSON] 
          | JObject [(String, JSON)]
          deriving (Show, Eq)

string :: Parser Char String
string = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

commaSep0 :: Parser Char a -> Parser Char [a]
commaSep0 p = p `sepBy0` (lit ',' <* ws)

json :: Parser Char JSON
json = JInt    <$> int
   <|> JBool   <$> bool
   <|> JNull   <$  lits "null"
   <|> JString <$> string
   <|> JList   <$  lit '[' <* ws <*> commaSep0 (json <* ws) <* lit ']'
   <|> JObject <$  lit '{' <* ws <*> commaSep0 ((,) <$> string <* ws <* lit ':' <* ws <*> json) <* ws <* lit '}'

jsonM :: Parser Char JSON
jsonM = jInt <|> jBool <|> jNull <|> jString <|> jList <|> jObject where
  jInt = do
    i <- int
    return $ JInt i
  jBool = do
    b <- bool
    return $ JBool b
  jNull = do
    lits "null"
    return JNull
  jString = do
    s <- string
    return $ JString s
  jList = do
    lit '[' 
    ws 
    xs <- commaSep0 jsonM
    ws 
    lit ')'
    return $ JList xs
  jObject = do
    lit '{' 
    ws
    items <- commaSep0 $ do
      key <- string
      ws
      lit ':'
      ws
      val <- jsonM
      return (key, val)
    ws 
    lit '}'
    return $ JObject items

json' :: Parser Char JSON
json' = ws *> json <* ws

prettyJson :: JSON -> String
prettyJson (JInt i) = show i
prettyJson (JBool True) = "true"
prettyJson (JBool False) = "false"
prettyJson (JString s) = "\"" ++ s ++ "\""
prettyJson JNull = "null"
prettyJson (JList xs) = "[" ++ intercalate ", " (map prettyJson xs) ++ "]"
prettyJson (JObject items) = "{" ++ intercalate ", " (map prettyItem items) ++ "}" where
  prettyItem (k, v) = "\"" ++ k ++ "\"" ++ ": " ++ prettyJson v

exJson :: String
exJson = unlines
  [ "{"
  , "  \"foo\": 42,"
  , "  \"bar\": [1, 2, false, null],"
  , "  \"baz\": \"boo\""
  , "}"
  ]

exParseJson :: [JSON]
exParseJson = runParserEnd json' exJson
