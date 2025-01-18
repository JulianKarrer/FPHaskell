{-# LANGUAGE GADTs #-}
module Main where
import Parser (Parser, satisfy, many1, many0, lit, runParserEnd)
import Data.Char (isAlpha)
import Control.Applicative ((<|>))
import Data.List (nub, delete, intercalate)
import Data.Maybe (fromMaybe)


-- data layout for Lambda expressions
data Expression where
    Var :: String -> Expression
    Lambda :: String -> Expression -> Expression
    App :: Expression -> Expression -> Expression
    deriving (Show, Eq)

-- parse lambda expressions
ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\r' <|> lit '\t')
ws1 :: Parser Char [Char]
ws1 = many1 (lit ' ' <|> lit '\n' <|> lit '\r' <|> lit '\t')
varParse :: Parser Char String
varParse = many1 (satisfy isAlpha)
exprParse :: Parser Char Expression
exprParse =
    (Var <$> varParse)
    <|> ( Lambda <$ lit '(' <* ws <* lit '\\' <* ws <*> varParse
        <* ws <* lit '.' <* ws <*> exprParse <* ws <* lit ')')
    <|> (App <$ lit '(' <* ws <*> exprParse <* ws1 <*> exprParse <* ws <* lit ')')

example1 :: Expression
example1 = head $ runParserEnd exprParse "(\\x . x)" -- expected: Lambda "x" (Var "x")

-- find free variables in expression
free :: Expression -> [String]
-- free expr = case expr of
free (Var name) = [name]
free (Lambda name exp1) = delete name (free exp1)
free (App exp1 exp2) = nub $ free exp1 ++ free exp2 -- delete.nub == remove !

-- create fresh variable names
freshVars :: [String]
-- freshVars = (\n -> "var" ++ show n) <$> [0::Integer ..]
freshVars = (show <$> ['a'..'z']) ++ ["var" ++ show n | n <- [0::Integer ..]]
fresh :: [String] -> String
fresh xs = head $ filter (`notElem` xs) freshVars

-- implement substitution of names for expressions
--  -> respect capture freedom!
(-->) :: String -> Expression -> Expression -> Expression
(from --> to) (Var name) | name == from = to
                         | otherwise = Var name
(from --> to) (App expr1 expr2) =
    App (from --> to $ expr1) (from --> to $ expr2)
(from --> to) (Lambda name expr)
  | name == from = Lambda name expr
  | name `notElem` free to = Lambda name (from --> to $ expr)
  | otherwise = let newname = fresh (free to ++ free expr) in
        Lambda newname (from --> to $ (name --> Var newname $ expr))

example2 :: Expression
example2 = "x" -->  Var "y" $ Lambda "z" (Var "x") -- expected: Lambda "z" (Var "y")


-- perform a single beta or eta reduction
-- returns the reduced expression and whether a reduction was found
step :: Expression -> Maybe Expression
step (App (Lambda x m) n) = Just $ x --> n $ m -- beta reduction
step (Lambda x (App m (Var x'))) | (x==x') && (x `notElem` free m) = Just m -- eta reduction
step (Var x) = Nothing -- no further reduction possible
step (Lambda name expr) = Lambda name <$> step expr --propagate into lambdas
step (App expr1 expr2) = -- propagate into applications
    ((`App` expr2) <$> step expr1) <|> (App expr1 <$> step expr2)
-- step (App expr1 expr2) = -- propagate into applications
--     case step expr1 of 
--         Just expr1' -> Just $ App expr1' expr2
--         Nothing -> case step expr2 of 
--             Just expr2' -> Just $ App expr1 expr2'
--             Nothing -> Nothing

-- perform steps while reductions are successful
steps :: Expression -> [Expression]
steps expr = case step expr of
    Just expr' -> expr : steps expr'
    Nothing -> [expr]

-- pretty printing
pretty :: Expression -> String
pretty (Var x) = x
pretty (Lambda name expr) = "(λ"++name++"."++pretty expr++")"
pretty (App expr1 expr2) = "("++pretty expr1++" "++pretty expr2++")"

-- REPL loop
main :: IO ()
main = do
    line <- getLine
    let parsed = runParserEnd exprParse line in
        if length parsed > 1 then
            putStrLn "Ambiguous parse of the input!"
        else if length parsed /= 1 then
            putStrLn "Invalid expression!"
        else
            putStrLn $ ">>>" ++ intercalate "\n>>>" (pretty <$> steps (head parsed))
    main

-- (((\x.(\y. (x x))) z) z)
-- ((\x. (x x)) (\x. (x x)))

