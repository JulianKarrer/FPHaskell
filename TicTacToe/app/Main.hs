module Main where

import Text.Read (readMaybe, Lexeme (Char))
import Data.List (intercalate, transpose)
import Data.Maybe (isNothing)

data Token = X | O | E deriving (Show, Eq)

type Board = [[Token]]

emptyBoard :: Int -> Board
emptyBoard n = replicate n (replicate n E)

isFull :: Board -> Bool
isFull = not . any (elem E)
-- all $ notElem E

showBoard :: Board -> String
showBoard b = intercalate "\n" (map showrow b) where
  showtoken x | x == E = "_"
              | otherwise = show x
  showrow xs = unwords (map showtoken xs) -- unwords = intercalate " "

updateListM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
updateListM i f xs | i<0 || i >= length xs   = Nothing -- bounds check
  | otherwise = case f (xs !! i) of
    Nothing -> Nothing
    Just res -> Just (zipWith (\elem ind -> if ind==i then res else elem) xs [0..])

setToken :: (Int, Int) -> Token -> Board -> Maybe Board
setToken (row, col) tok = updateListM row updaterow where
  updaterow = updateListM col (\t -> if t==E then Just tok else Nothing)

winner :: Board -> Token
winner b | won X = X
         | won O = O
         | otherwise = E
  where
  rows :: Token -> Board -> Bool
  rows token = any (all (== token))
  diagonal :: Token -> Board -> Bool
  diagonal t board = and (zipWith (\index row -> row !! index == t) [0..] board)
  won :: Token -> Bool
  won t = rows t b || rows t (transpose b) || diagonal t b || diagonal t (reverse b)

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
