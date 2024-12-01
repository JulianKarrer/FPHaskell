module Main where

import Test.QuickCheck
import Data.Int (Int)

myFunction :: Int->Int
myFunction x=x+1

prop_myFunction :: Int->Bool
prop_myFunction x = myFunction x == x+1

main :: IO ()
main = quickCheck prop_myFunction
