{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
module Main where

import Text.Read (readMaybe)
import Data.List (intercalate, transpose)

data Token = X | O | E deriving (Eq)

instance Show Token where
  show t = case t of
    X -> "X"
    O -> "O"
    E -> "_"

type Board = [[Token]]

emptyBoard :: Int -> Board
emptyBoard n = replicate n (replicate n E)

isFull :: Board -> Bool
isFull = all (notElem E)

showBoard :: Board -> String
showBoard b = intercalate "\n" $
  map (intercalate " " . map show) b

mapAtM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
mapAtM i f (x:xs) | i==0 = case f x of
  Just res -> Just $ res : xs
  Nothing -> Nothing
mapAtM i f (x:xs) | i>0 = case mapAtM (i-1) f xs of
  Just xs' -> Just $ x : xs'
  Nothing -> Nothing
mapAtM _ _ _ = Nothing

setToken :: (Int, Int) -> Token -> Board -> Maybe Board
setToken (c,r) t =
    mapAtM r (
      mapAtM c (
        \tcur -> if tcur == E then Just t else Nothing
      )
    )


winner :: Board -> Token
winner b
  | won X && won O = E -- should not happen
  | won X = X
  | won O = O
  | otherwise = E
  where
    getM :: Int -> [a] -> Maybe a
    getM i (x:xs) | i ==0     = Just x
                  | i > 0     = getM (i-1) xs
    getM _ _ = Nothing
    ni :: Int
    ni = length b - 1
    won :: Token -> Bool
    won t =
      or [all (==t) row | row <- b] ||           -- full row 
      or [all (==t) col | col <- transpose b] || -- full column
      all (== Just (Just t)) [ getM i      <$> getM i b | i <- [0..ni]] || 
      all (== Just (Just t)) [ getM (ni-i) <$> getM i b | i <- [0..ni]]


main :: IO ()
main = run (emptyBoard 3) X where

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
