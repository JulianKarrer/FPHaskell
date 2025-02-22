{-# LANGUAGE FlexibleContexts #-}

module Ex08Solution2 where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List (intercalate)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

getLine' :: MonadIO m => m String
getLine' = liftIO getLine

--------------------------------------------------------------------------------

todo' :: IO ()
todo' = evalStateT run [] where

  run :: (MonadState [String] m, MonadIO m) => m ()
  run = do
    putStrLn' ""
    c <- choose "What do you want to do?" ["add", "display", "remove", "quit"]
    case c of
      "add" -> do
        putStrLn' "Enter text of todo item:"
        item <- getLine'
        modify (item:)
        run
      "display" -> do
        items <- get
        putStrLn' ""
        putStrLn' $ intercalate "\n"
                  $ map (\(i,s) -> show i ++ ") " ++ s)
                  $ enumerate items
        run
      "remove" -> do
        putStrLn' "Index of item to remove:"
        s <- getLine'
        case readMaybe s of
          Nothing -> do
            putStrLn' "Invalid index."
            run
          Just i -> do
            items <- get
            case removeAt i items of
              Nothing -> do
                putStrLn' "Invalid index."
                run
              Just (item, items') -> do
                put items'
                putStrLn' $ "Removed item: " ++ item
                run
      "quit" ->
        return ()
      _ -> error "IMPOSSIBLE"

  choose :: MonadIO m => String -> [String] -> m String
  choose question options = do
    putStrLn' $ question ++ " [" ++ intercalate "|" options ++ "]"
    l <- getLine'
    if l `elem` options then
      return l
    else do
      putStrLn' "Invalid input. Try again!"
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


type EvalM a = ExceptT RuntimeError (StateT Env IO) a

lookupM :: (MonadError RuntimeError m, MonadState Env m) => Var -> m Val
lookupM x = do
  env <- get
  case lookup x env of
    Just v -> return v
    Nothing -> throwError $ UndefinedVar x

eval :: (MonadError RuntimeError m, MonadState Env m, MonadIO m) => Expr -> m Val
eval (EVar x) = lookupM x
eval (EVal v) = return v
eval (EOp e1 o e2) = do
  v1 <- eval e1
  v2 <- eval e2
  semOp o v1 v2
eval (EAssign x e) = do
  v <- eval e
  modify $ insertOrUpdate x v
  return VUnit
eval (EWhile e1 e2) = do
  v1 <- eval e1
  case v1 of
    VBool False -> return VUnit
    VBool True  -> eval (ESeq e2 (EWhile e1 e2))
    _           -> throwError $ TypeError "While condition was not a bool"
eval (ESeq e1 e2) = do
  eval e1
  eval e2
eval (EPrint e) = do
  v <- eval e
  print' v
  return VUnit

