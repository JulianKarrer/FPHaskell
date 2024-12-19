{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Trans.State.Strict
import Data.List (intercalate)
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)


-- TODO LIST 

main :: IO ()
-- main = todo

choose :: String -> [String] -> IO String
choose question answers = do
    putStrLn (question ++ " [" ++ intercalate "|" answers ++ "]")
    l <- getLine
    if l `elem` answers then
      return l
    else do
      putStrLn "Invalid input. Try again!"
      choose question answers
enum :: [a] -> [(Int, a)]
enum = zip [0..]

todo :: IO ()
todo = evalStateT run [] where
    run :: StateT [String] IO ()
    run = do
        selection <- lift $ choose
            "What do you want to do?"
            ["add", "display", "remove", "quit"]
        case selection of
            "add" -> do
                lift $ putStrLn "Enter text of todo item:"
                newitem <- lift getLine
                modify (newitem:)
                run
            "display" -> do
                todolist <- get
                lift $ putStrLn $ foldr
                    (\(index, item) str -> str ++ "\n" ++  (show index ++ ") " ++ item))
                    ""
                    (reverse $ enum todolist)
                run
            "remove" -> do
                lift $ putStrLn "Index of item to remove:"
                remline <- lift getLine
                case readMaybe remline of
                    Nothing -> do
                        lift $ putStrLn "Invalid index"
                        run
                    Just rid -> do
                        todolist <- get
                        if 0 <= rid && rid < length todolist then do
                            lift $ putStrLn ("Removed item: " ++ todolist !! rid)
                            put (map snd $ filter (\(i, _) -> i /= rid) (enum todolist))
                            run
                        else do
                            lift $ putStrLn "Index out of Bounds"
                            run
            "quit" -> return ()
            _ -> return ()


-- WHILE INTERPRETER
-- Variables
type Var = String
-- Values
data Val = VInt Int
         | VBool Bool
         | VUnit
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
          | EPrint Expr
          deriving (Eq, Show)
-- Environments
type Env = [(Var, Val)]
data RuntimeError = UndefinedVar String
                  | TypeError String deriving (Eq, Show)
-- Semantics of an operator (how to apply an operator to values)
semOp :: Op -> Val -> Val -> Either RuntimeError Val
semOp Add  (VInt x)   (VInt y)   = Right $ VInt (x + y)
semOp Sub  (VInt x)   (VInt y)   = Right $ VInt (x - y)
semOp Less (VInt x)   (VInt y)   = Right $ VBool (x < y)
semOp _    _          _          = Left $ TypeError "Operator cannot handle those arguments"
insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x'   = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env

-- Evaluation
type EvalM a = ExceptT RuntimeError (StateT Env IO) a
-- Semantics of an operator (how to apply an operator to values)
eval :: Expr -> EvalM Val
eval (EVar x) = do
    env <- lift get
    case lookup x env of
        Just v -> return v
        Nothing -> throwE $ UndefinedVar x
eval (EVal v) = do
    return v
eval (EOp e1 o e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case semOp o v1 v2 of
        Left err -> throwE err
        Right res -> return res
eval (EAssign x e) = do
    v <- eval e
    lift $ modify (insertOrUpdate x v)
    return VUnit
eval self@(EWhile e1 e2) = do
    v1 <- eval e1
    case v1 of
        VBool False -> return VUnit
        VBool True -> eval (ESeq e2 self)
        _ -> throwE $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) = eval e1 *> eval e2
eval (EPrint e) = do
    v <- eval e
    lift $ lift $ print v
    return VUnit

printEval :: EvalM Val -> IO ()
printEval e = do
    result <- evalStateT (runExceptT e) []
    case result of
        Left err -> print err
        Right val -> print val

testExpression :: Expr
testExpression = ESeq
    (EPrint $ EOp (EVal $ VInt 42) Add (EVal $ VInt 69))
    (EPrint $ EOp (EVal $ VInt 9) Add (EVal $ VInt 10))

main = printEval $ eval testExpression

