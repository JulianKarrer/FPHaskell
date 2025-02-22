{-# LANGUAGE FlexibleContexts #-}
module Main where
import Control.Monad.IO.Class
import Data.List (intercalate)
import Control.Monad.State.Strict ( StateT, evalStateT, modify, get, put, MonadState )
import Text.Read (readMaybe)
import Control.Monad.Except

main :: IO ()
-- main = todo

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn
print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print
getLine' :: MonadIO m => m String
getLine' = liftIO getLine

choose :: String -> [String] -> StateT [String] IO String
choose question answers = do
    putStrLn' (question ++ " [" ++ intercalate "|" answers ++ "]")
    l <- getLine'
    if l `elem` answers then
      return l
    else do
      putStrLn' "Invalid input. Try again!"
      choose question answers
enum :: [a] -> [(Int, a)]
enum = zip [0..]

todo :: IO ()
todo = evalStateT run [] where
    run :: StateT [String] IO ()
    run = do
        selection <- choose "What do you want to do?" ["add", "display", "remove", "quit"]
        case selection of
            "add" -> do
                putStrLn' "Enter text of todo item:"
                newitem <- getLine'
                modify (newitem:)
                run
            "display" -> do
                todolist <- get
                putStrLn' $ foldr
                    (\(index, item) str -> str ++ "\n" ++  (show index ++ ") " ++ item))
                    ""
                    (reverse $ enum todolist)
                run
            "remove" -> do
                putStrLn' "Index of item to remove:"
                remline <- getLine'
                case readMaybe remline of
                    Nothing -> do
                        putStrLn' "Invalid index"
                        run
                    Just rid -> do
                        todolist <- get
                        if 0 <= rid && rid < length todolist then do
                            putStrLn' ("Removed item: " ++ todolist !! rid)
                            put (map snd $ filter (\(i, _) -> i /= rid) (enum todolist))
                            run
                        else do
                            putStrLn' "Index out of Bounds"
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
semOp :: MonadError RuntimeError m => Op -> Val -> Val -> m Val
semOp Add  (VInt x)   (VInt y)   = return $ VInt (x + y)
semOp Sub  (VInt x)   (VInt y)   = return $ VInt (x - y)
semOp Less (VInt x)   (VInt y)   = return $ VBool (x < y)
semOp _    _          _          = throwError $ TypeError "Operator cannot handle those arguments"
insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x'   = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env

lookupM :: (MonadError RuntimeError m, MonadState Env m) => Var -> m Val
lookupM x = do
    env <- get
    case lookup x env of
        Just v -> return v
        Nothing -> throwError $ UndefinedVar x

-- Evaluation
eval :: (MonadError RuntimeError m, MonadState Env m, MonadIO m) => Expr -> m Val
eval (EVar x) = lookupM x
eval (EVal v) = do
    return v
eval (EOp e1 o e2) = do
    v1 <- eval e1
    v2 <- eval e2
    semOp o v1 v2
eval (EAssign x e) = do
    v <- eval e
    modify $ insertOrUpdate x v
    return VUnit
eval self@(EWhile e1 e2) = do
    v1 <- eval e1
    case v1 of
        VBool False -> return VUnit
        VBool True -> eval (ESeq e2 self)
        _ -> throwError $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) = eval e1 *> eval e2
eval (EPrint e) = do
    v <- eval e
    print' v
    return VUnit

type EvalM a = ExceptT RuntimeError (StateT Env IO) a
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


