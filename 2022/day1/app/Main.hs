module Main where

import Data.List.Split
import Data.List
import Data.Ord

buildblocks :: [String] -> [[String]]
buildblocks = splitOn [""] 

getElfInventories :: String -> [[String]]
getElfInventories = splitOn [""] <$> splitOn "\n"

countCalories :: [String] -> Integer
countCalories foods = sum $ map read foods 

solvePart1 :: String -> Integer
solvePart1 input = maximum $ countCalories <$> getElfInventories input

solvePart2 :: String -> Integer
solvePart2 input = sum . take 3 . sortBy (comparing Down) $ countCalories <$> getElfInventories input

main :: IO ()
main = do
    input <- getContents 
    let soln1 = solvePart1 input
    let soln2 = solvePart2 input
    putStrLn (show soln1)
    putStrLn (show soln2)
    