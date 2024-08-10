module Main where

import Control.Applicative
import Data.List (sortOn)
import Data.List.Split (linesBy)
import Data.Ord (Down (Down))
import GHC.Read (readNumber)
import Paths_advent (getDataFileName)
import Text.Parsec
import Text.ParserCombinators.Parsec.Number

foodCalories = do
  n <- many1 digit
  return (read n)

foodInventory = sepBy foodCalories (char '\n')

input = endBy foodInventory eof

countInventoryCalories :: [String] -> Integer
countInventoryCalories foods = sum $ caloriesIn <$> foods
  where
    caloriesIn = read

filename = "input/day1.txt"

main :: IO ()
main = do
  content <- getDataFileName filename >>= readFile
  case parse input filename content of
    Left err -> print err
    Right inventories -> do
      print inventories
      print (soln1, soln2)
      where
        calories = sortOn Down $ countInventoryCalories <$> inventories
        soln1 = head calories
        soln2 = sum $ take 3 calories
