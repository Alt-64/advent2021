module Main where

import Data.List.Split
import Data.List
import Data.Ord

getCalories :: String -> [Integer]
getCalories = fmap countCalories . getInventories 

getInventories :: String -> [[String]]
getInventories = linesBy (=="") <$> lines

countCalories :: [String] -> Integer
countCalories foods = sum $ caloriesIn <$> foods 
    where caloriesIn = read

main :: IO ()
main = do
    calories <- (sortBy (comparing Down) . getCalories) <$> getContents
    putStrLn . show . head $ calories       
    putStrLn . show . sum $ take 3 calories 
