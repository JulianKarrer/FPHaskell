{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where
import Data.List (intercalate, delete)
import Text.Read (readMaybe)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

main :: IO ()
main = runEval (eval (EVal (VInt 42)))

todo :: IO ()
todo = evalStateT run []

-- EXERCISE 1.2
getSafe :: Int -> [a] -> Maybe a
getSafe i (_:xs) | i > 0 = getSafe (i-1) xs
getSafe 0 (x:_) = Just x
getSafe _ _ = Nothing

nats :: [Integer]
nats = [0..]

choose :: String -> [String] -> IO String
choose question answers = do
    putStrLn $ question ++ "[" ++ intercalate "|" answers ++ "]"
    l <- getLine
    if l `elem` answers then
        return l
    else do
        putStrLn "Invalid input. Try again!"
        choose question answers

actions :: [String]
actions = ["add", "display", "remove", "quit"]

run :: StateT [String] IO ()
run = do
    lift $ putStrLn ""
    choice <- lift $ choose "What do you want to do? " actions
    case choice of
        "add" -> do
            lift $ putStrLn "Enter text of todo item:"
            added <- lift getLine
            modify (added:)
            run
        "display" -> do
            items <- get
            lift $ foldr
                (\(l, i) p -> p >> putStrLn (show i ++ ") " ++ l))
                (putStrLn "") --(pure ()) 
                (reverse (zip (reverse items) nats))
            run
        "remove" -> do
            lift $ putStrLn "Index of item to remove:"
            l <- lift  getLine
            case readMaybe l :: Maybe Int of
                Nothing -> do
                    lift $ putStrLn "Input is not a number!"
                    run
                Just n -> do
                    items <- get
                    case getSafe n items of
                        Just item -> do
                            modify (delete item)
                            run
                        Nothing -> do
                            lift $ putStrLn "Invalid index!"
                            run
        "quit" -> return ()
        _ -> do -- should be impossible
            lift $ putStrLn "ERROR"
            return ()

-- EXERCISE 1.3

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
                  | TypeError String deriving Show

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


type EvalM a = ExceptT RuntimeError (StateT Env IO) a

eval :: Expr -> EvalM Val
eval (EVar x) = do
    env <- get
    case lookup x env of
        Just v -> return v
        Nothing -> throwError $ UndefinedVar x
eval (EVal v) = return v
eval (EOp e1 o e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case semOp o v1 v2 of
        Left err -> throwError err
        Right v -> return v
eval (EAssign x e) = do
    v <- eval e
    modify (insertOrUpdate x v)
    return VUnit
eval (EWhile e1 e2) = do
    v1 <- eval e1
    case v1 of
        VBool False -> return VUnit
        VBool True  -> eval (ESeq e2 (EWhile e1 e2))
        _           -> throwError $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) = eval e1 >> eval e2
eval (EPrint e) = do
    v <- eval e
    lift $ lift $ print v
    return VUnit

runEval :: EvalM Val -> IO ()
runEval m = do
    (runStateT $ runExceptT m) [] >>= \x -> case x of
        (Left err, env) -> print (err, env)
        (Right val, env) -> print (val, env)
