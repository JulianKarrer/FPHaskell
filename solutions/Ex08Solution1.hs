{-# LANGUAGE FlexibleContexts #-}

module Ex08Solution1 where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.List (intercalate)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

todo :: IO ()
todo = evalStateT run [] where

  run :: StateT [String] IO ()
  run = do
    lift $ putStrLn ""
    c <- lift $ choose "What do you want to do?" ["add", "display", "remove", "quit"]
    case c of
      "add" -> do
        lift $ putStrLn "Enter text of todo item:"
        item <- lift getLine
        modify (item:)
        run
      "display" -> do
        items <- get
        lift $ putStrLn ""
        lift $ putStrLn $ intercalate "\n"
                        $ map (\(i,s) -> show i ++ ") " ++ s)
                        $ enumerate items
        run
      "remove" -> do
        lift $ putStrLn "Index of item to remove:"
        s <- lift getLine
        case readMaybe s of
          Nothing -> do
            lift $ putStrLn "Invalid index."
            run
          Just i -> do
            items <- get
            case removeAt i items of
              Nothing -> do
                lift $ putStrLn "Invalid index."
                run
              Just (item, items') -> do
                put items'
                lift $ putStrLn $ "Removed item: " ++ item
                run
      "quit" ->
        return ()
      _ -> error "IMPOSSIBLE"

  choose :: String -> [String] -> IO String
  choose question options = do
    putStrLn $ question ++ " [" ++ intercalate "|" options ++ "]"
    l <- getLine
    if l `elem` options then
      return l
    else do
      putStrLn "Invalid input. Try again!"
      choose question options

  enumerate :: [a] -> [(Int, a)]
  enumerate = zip [0..]

  removeAt :: Int -> [a] -> Maybe (a, [a])
  removeAt _ [] = Nothing
  removeAt 0 (x : xs) = Just (x, xs)
  removeAt i (x : xs) = case removeAt (i-1) xs of
    Nothing -> Nothing
    Just (y, ys) -> Just (y, x:ys)

--------------------------------------------------------------------------------

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
                  | TypeError String

-- Semantics of an operator (how to apply an operator to values)
semOp :: Op -> Val -> Val -> EvalM Val
semOp Add  (VInt x)   (VInt y)   = return $ VInt (x + y)
semOp Sub  (VInt x)   (VInt y)   = return $ VInt (x - y)
semOp Less (VInt x)   (VInt y)   = return $ VBool (x < y)
semOp _    _          _          = throwE $ TypeError "Operator cannot handle those arguments"

insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x'   = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env


type EvalM a = ExceptT RuntimeError (StateT Env IO) a

lookupM :: Var -> EvalM Val
lookupM x = do
  env <- lift get
  case lookup x env of
    Just v -> return v
    Nothing -> throwE $ UndefinedVar x

eval :: Expr -> EvalM Val
eval (EVar x) = lookupM x
eval (EVal v) = return v
eval (EOp e1 o e2) = do
  v1 <- eval e1
  v2 <- eval e2
  semOp o v1 v2
eval (EAssign x e) = do
  v <- eval e
  lift $ modify $ insertOrUpdate x v
  return VUnit
eval (EWhile e1 e2) = do
  v1 <- eval e1
  case v1 of
    VBool False -> return VUnit
    VBool True  -> eval (ESeq e2 (EWhile e1 e2))
    _           -> throwE $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) = do
  eval e1
  eval e2
eval (EPrint e) = do
  v <- eval e
  lift $ lift $ print v
  return VUnit

