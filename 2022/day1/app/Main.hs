module Main where

import Data.List.Split
import Data.List
import Data.Ord

type Food = String
type Calories = Integer

getCalories :: String -> [Calories]
getCalories input = countCalories <$> getInventories input

getInventories :: String -> [[Food]]
getInventories = splitOn [""] <$> splitOn "\n"

countCalories :: [Food] -> Calories
countCalories foods = sum $ caloriesIn <$> foods 

caloriesIn :: Food -> Calories
caloriesIn = read 

main :: IO ()
main = do
    calories <- (sortBy (comparing Down) . getCalories) <$> getContents
    putStrLn (show $ head $ calories)           -- Part 1
    putStrLn (show $ sum . take 3 $ calories)   -- Part 2
