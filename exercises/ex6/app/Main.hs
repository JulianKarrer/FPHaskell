{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Main where
import Data.Maybe (isNothing, fromMaybe)
import Data.Char (isAlpha, isDigit)
import WhileInterp

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- EXERCISE 1

newtype ND a = ND [a]
instance Functor ND where
    fmap :: (a -> b) -> ND a -> ND b
    fmap f (ND a) = ND (map f a)
instance Applicative ND where
    pure :: a -> ND a
    pure = undefined
    (<*>) :: ND (a -> b) -> ND a -> ND b
    (<*>) = undefined
instance Monad ND where
    return :: a -> ND a
    return x = ND [x]
    (>>=) :: ND a -> (a -> ND b) -> ND b
    (ND list) >>= f = ND (concatMap (runND . f) list)

runND :: ND a -> [a]
runND (ND list) = list
choose :: [a] -> ND a
choose = ND
abort :: ND a
abort = ND []

ex1 :: [Int]
ex1 = runND $ do
    x <- choose [1, 2]
    y <- choose [10, 20]
    return $ x + y

ex2 :: [Int]
ex2 = runND $ do
    x <- choose [1..10]
    if even x then
        return x
    else
        abort

flipCoin :: ND Bool
flipCoin = choose [True, False]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = do
    c1 <- flipCoin
    c2 <- flipCoin
    return (c1, c2)

type Graph n = [(n, [n])]
type Coloring n c = [(n, c)]


-- way easier option available -> see example solution
solve :: (Eq n, Eq c) => Graph n -> [c] -> ND (Coloring n c)
solve g colours = solve' g colours [] where
    solve' :: (Eq n, Eq c) => Graph n -> [c] -> Coloring n c -> ND (Coloring n c)
    solve' graph colors coloring =
        let uncoloured = filter (\n -> isNothing (lookup n coloring)) $ map fst graph in
        if not $ null uncoloured then
            let node = head uncoloured in
            let nbrs = fromMaybe [] (lookup node graph) in
            let nbrsCols = foldr (\nbr cols -> cols ++ getColours nbr coloring) [] nbrs in
            let availableCols = filter (`notElem` nbrsCols) colors in
            if null availableCols then abort else do
                colour <- choose availableCols
                solve' graph colors ((node, colour) : coloring)
        else return coloring
    getColours :: (Eq n, Eq c) => n -> Coloring n c -> [c]
    getColours node coloring = case lookup node coloring of
        Just c -> [c]
        Nothing -> []

exGraph :: Graph Int
exGraph = -- 1
    [ (0, [1,2]) -- / \
    , (1, [3,0]) -- 0 3
    , (2, [3,0]) -- \ /
    , (3, [1,2]) -- 2
    ]
exColorings :: [Coloring Int String]
exColorings = runND $ solve exGraph ["red", "blue"]

-- EXERCISE 2

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
    pure = undefined
    (<*>) :: Partial (a -> b) -> Partial a -> Partial b
    (<*>) = undefined
instance Monad Partial where
    (>>=) :: Partial a -> (a -> Partial b) -> Partial b
    (Partial mayb) >>= f = case mayb of
        Just val -> f val
        Nothing -> Partial Nothing
    return :: a -> Partial a
    return x = Partial $ Just x
failure :: Partial a
failure = Partial Nothing

(!?) :: [a] -> Int -> Partial a
-- xs !? index =
--     if 0 <= index && index < length xs then Partial $ Just $ xs !! index
--     else failure
[] !? _ = failure
[x] !? 0 = return x
(x:xs) !? i = xs !? (i-1)

getCell :: [[a]] -> Int -> Int -> Partial a
getCell matrix c r = do
    row <- matrix !? r
    row !? c

-- EXERCISE 3
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
  pure = undefined
  (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (<*>) = undefined
instance Monad (Exception e) where
  (>>=) :: Exception e a -> (a -> Exception e b) -> Exception e b
  (Exception e) >>= f = case e of
    Left e -> Exception (Left e)
    Right a ->  f a
  return :: a -> Exception e a
  return x = Exception (Right x)

raise :: e -> Exception e a
raise exception = Exception (Left exception)
withException :: Partial a -> e -> Exception e a
withException partial exception = case runPartial partial of
    Just value -> Exception $ Right value
    Nothing -> Exception $ Left exception

validatePassword :: String -> Exception String ()
validatePassword pwd
  | length pwd < 8 = raise "Password too short! (< 8 symbols)"
  | not $ any isDigit pwd = raise "Password does not contain a number!"
  | not $ any isAlpha pwd = raise "Password does not contain an alphabetic character!"
  | otherwise = return ()

data MatrixError = InvalidRowIndex | InvalidColIndex deriving (Show)
getCell' :: [[a]] -> Int -> Int -> Exception MatrixError a
getCell' matrix r c = do
    row <- withException (matrix !? r) InvalidRowIndex
    withException (row !? c) InvalidColIndex

-- EXERCISE 4
newtype State s a = State (s -> (a, s))
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State ((\(a, s) -> (f a, s)) . g)
instance Applicative (State s) where
    pure :: a -> State s a
    pure = undefined
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) = undefined
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State g >>= f = State $ \s ->
    let (res, newState) = g s in
    --     (State stateAfterF) = f res
    -- in stateAfterF newState
        runState (f res) newState
  return :: a -> State s a
  return x = State $ \s -> (x,s)

runState :: State s a -> s -> (a, s)
runState (State s) = s
get :: State s s
get = State $ \s -> (s,s)
put :: s -> State s ()
put x = State $ const ((), x)
modify :: (s -> s) -> State s ()
modify f = do 
    val <- get 
    put (f val)

printEval :: State Env Val -> (Val, Env)
printEval state = runState state []

eval' :: Expr -> State Env Val
eval' (EVar x) = do
    env <- get
    case lookup x env of
        Just v -> return v
        Nothing -> return $ VError("Variable '" ++ x ++ "' is not defined.")
eval' (EVal v) = return v
eval' (EOp e1 o e2) = do
    v1 <- eval' e1 
    v2 <- eval' e2
    return $ semOp o v1 v2
eval' (EAssign x e) = do
    v <- eval' e
    env <- get
    put (insertOrUpdate x v env)
    -- or: replace get + put with
    -- modify $ insertOrUpdate x v
    return VUnit
eval' (EWhile e1 e2) = do
    v1 <- eval' e1
    case v1 of
        VBool False -> return VUnit
        VBool True  -> eval' (ESeq e2 (EWhile e1 e2))
        VError e    -> return $ VError e
        _           -> return $ VError "While condition was not a bool"
eval' (ESeq e1 e2) = do
    v1 <- eval' e1
    eval' e2

example :: Expr
example = ESeq (EAssign "x" $ EVal $ VInt 0)
    (ESeq (EWhile (EOp (EVar "x") Less (EVal $ VInt 10))
    (EAssign "x" $ EOp (EVar "x") Add (EVal $ VInt 1)))
    (EOp (EVar "x") Add (EVal $ VInt 5)))

