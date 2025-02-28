
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Control.Monad.State
import Data.List (intercalate, delete)
import Text.Read (readMaybe)
import Control.Monad.Except

main :: IO ()
main = putStrLn "Hello, Haskell!"

todo :: IO ()
todo = evalStateT run []

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

printState :: (MonadState Int m, MonadIO m) => m ()
printState = do
    s <- get
    liftIO (print s)

run :: (MonadState [String] m, MonadIO m) => m ()
run = do 
    liftIO $ putStrLn ""
    choice <- liftIO $ choose "What do you want to do? " actions
    case choice of
        "add" -> do
            liftIO $ putStrLn "Enter text of todo item:"
            added <- liftIO getLine
            modify (added:)
            run
        "display" -> do
            items <- get
            liftIO $ foldr
                (\(l, i) p -> p >> putStrLn (show i ++ ") " ++ l))
                (putStrLn "") --(pure ()) 
                (reverse (zip (reverse items) nats))
            run
        "remove" -> do
            liftIO $ putStrLn "Index of item to remove:"
            l <- liftIO getLine
            case readMaybe l :: Maybe Int of
                Nothing -> do
                    liftIO $ putStrLn "Input is not a number!"
                    run
                Just n -> do
                    items <- get
                    case getSafe n items of
                        Just item -> do
                            modify (delete item)
                            run
                        Nothing -> do
                            liftIO $ putStrLn "Invalid index!"
                            run
        "quit" -> return ()
        _ -> do -- should be impossible
            liftIO $ putStrLn "ERROR"
            return ()



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

type EvalM = ExceptT RuntimeError (StateT Env IO)

eval ::  Expr -> EvalM Val
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
    liftIO $ print v
    return VUnit

runEvalEx :: IO ()
runEvalEx = do
  result <- evalStateT (runExceptT (eval $ EVal $ VInt 42  )) []
  case result of
    Left err  -> print err
    Right val -> print val

