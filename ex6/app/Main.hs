{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Main where
import Data.Maybe (isNothing, fromMaybe)

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
flipCoin = ND [True, False]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = do
    c1 <- flipCoin
    c2 <- flipCoin
    return (c1, c2)

type Graph n = [(n, [n])]
type Coloring n c = [(n, c)]

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
xs !? index = 
    if 0 <= index && index < length xs then Partial $ Just $ xs !! index 
    else failure

getCell :: [[a]] -> Int -> Int -> Partial a
getCell matrix r c = do
    row <- matrix !? r
    row !? c




