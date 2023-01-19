module Main where

import Data.List.Split
import Data.List
import Data.Ord

getTotalCalories :: String -> [Integer]
getTotalCalories = fmap countCalories . getInventories 

getInventories :: String -> [[String]]
getInventories = linesBy (=="") <$> lines

countCalories :: [String] -> Integer
countCalories foods = sum $ caloriesIn <$> foods 
    where caloriesIn = read

main :: IO ()
main = do
    input <- getContents
    let calories = sortBy (comparing Down) . getTotalCalories $ input
    putStrLn . show . head $ calories 
    putStrLn . show . sum $ take 3 calories 
