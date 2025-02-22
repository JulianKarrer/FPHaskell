{-# LANGUAGE LambdaCase #-}

module Ex02Solution where

import Data.List
import Text.Read (readMaybe)

-- map, filter, and fold -------------------------------------------------------

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []                   = []
filter' f (x : xs) | f x       = x : filter' f xs
                   | otherwise = filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ y []       = y
foldr' f y (x : xs) = f x (foldr' f y xs)

inc :: [Int] -> [Int]
inc []       = []
inc (x : xs) = (x + 1) : inc xs

inc' :: [Int] -> [Int]
inc' = map' (+1)

evenList :: [Int] -> [Int]
evenList []                   = []
evenList (x : xs) | even x    = x : evenList xs
                  | otherwise = evenList xs

evenList' :: [Int] -> [Int]
evenList' = filter even

shortStrs :: [Int] -> [String]
shortStrs [] = []
shortStrs (x : xs) | length (show x) <= 2 = show x : shortStrs xs
                   | otherwise            = shortStrs xs

shortStrs' :: [Int] -> [String]
shortStrs' = filter (\s -> length s <= 2) . map show

and' :: [Bool] -> Bool
and' []       = True
and' (x : xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' = foldr' (&&) True

or' :: [Bool] -> Bool
or' []       = False
or' (x : xs) = x || or' xs

or'' :: [Bool] -> Bool
or'' = foldr' (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' _ []       = True
all' f (x : xs) = f x && all' f xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr' (\x b -> f x && b) True

all''' :: (a -> Bool) -> [a] -> Bool
all''' f = foldr' (&&) True . map f

any' :: (a -> Bool) -> [a] -> Bool
any' _ []       = False
any' f (x : xs) = f x || all' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = foldr' (\x b -> f x || b) False

any''' :: (a -> Bool) -> [a] -> Bool
any''' f = foldr' (||) False . map f

length' :: [a] -> Int
length' []       = 0
length' (_ : xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> n + 1) 0

idMap :: [a] -> [a]
idMap = map (\x -> x)

idMap' :: [a] -> [a]
idMap' = map id

idFold :: [a] -> [a]
idFold = foldr' (:) []

idFilter :: [a] -> [a]
idFilter = filter (\_ -> True)

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x ys -> f x : ys) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x ys -> if f x then x : ys else ys) []

-- V3 --------------------------------------------------------------------------

data V3 a = V3 a a a deriving (Show, Eq)

mapV3 :: (a -> b) -> (V3 a -> V3 b)
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

liftV3 :: (a -> b -> c) -> (V3 a -> V3 b -> V3 c)
liftV3 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

negV3 :: V3 Int -> V3 Int
negV3 = mapV3 negate
-- negV3 = mapV3 (\x -> -x)

addV3 :: V3 Int -> V3 Int -> V3 Int
addV3 = liftV3 (+)

subV3 :: V3 Int -> V3 Int -> V3 Int
subV3 = liftV3 (-)

mulV3 :: V3 Int -> V3 Int -> V3 Int
mulV3 = liftV3 (*)

divV3 :: V3 Int -> V3 Int -> V3 Int
divV3 = liftV3 div

-- Tic Tac Toe -----------------------------------------------------------------

data Token = X | O | E deriving (Show, Eq)

type Board = [[Token]]

showTok :: Token -> String
showTok X = "X"
showTok O = "O"
showTok E = "_"

showRow :: [Token] -> String
showRow ts = intercalate " " $ map showTok ts

showBoard :: Board -> String
showBoard b = intercalate "\n" $ map showRow b

mapAtM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
mapAtM _ _ []       = Nothing
mapAtM 0 f (x : xs) = fmap (:xs) (f x)
mapAtM i f (x : xs) = fmap (x:) (mapAtM (i - 1) f xs)

setToken :: (Int, Int) -> Token -> Board -> Maybe Board
setToken (x, y) t = mapAtM y $ mapAtM x $ \case
  E -> Just t
  _ -> Nothing

-- Extract the tokens on the diagonal line from top-left to bottom-right
diagonal :: Board -> [Token]
diagonal b = go b 0 where
  go []           _ = []
  go (row : rows) i = (row !! i) : go rows (i + 1)

winner :: Board -> Token
winner b = diag `orTok` horizontal b `orTok` vertical b
  where
    d1 = diagonal b
    d2 = diagonal (reverse b)

    diag :: Token
    diag = win d1 `orTok` win d2

    horizontal :: Board -> Token
    horizontal = foldr orTok E . map win

    vertical :: Board -> Token
    vertical b' = horizontal (transpose b')

    win :: [Token] -> Token
    win ts = if not (null ts) && all (== head ts) ts
               then head ts
               else E

    orTok :: Token -> Token -> Token
    orTok E t = t
    orTok t _ = t
    
emptyBoard :: Int -> Board
emptyBoard n = replicate n $ replicate n E

testBoard :: Board
testBoard = 
  [ [ X, E, E ]
  , [ E, O, E ]
  , [ E, O, E ]
  ]

isFull :: Board -> Bool
isFull = all (notElem E)

tictactoe :: IO ()
tictactoe = run (emptyBoard 3) X where

  run :: Board -> Token -> IO ()
  run b t = do
    putStrLn $ showBoard b ++ "\n"
    putStr $ "Where to place " ++ show t ++ "? "
    line <- getLine
    case readMaybe line of
      Nothing -> do
        putStrLn "Invalid input... Try again!"
        run b t
      Just pos -> do
        putStrLn ""
        case setToken pos t b of
          Nothing -> do
            putStrLn "Invalid position... Try again!"
            run b t
          Just b' -> do
            case winner b' of
              E | isFull b' -> do
                putStrLn $ showBoard b' ++ "\n"
                putStrLn "Game over. Everyone looses."
              E -> do
                run b' (flipTok t)
              t' -> do
                putStrLn $ showBoard b' ++ "\n"
                putStrLn $ show t' ++ " is the winner!"

  flipTok :: Token -> Token
  flipTok X = O
  flipTok O = X
  flipTok E = E
