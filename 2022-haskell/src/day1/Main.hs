module Main where

import Data.List.Split ( linesBy )
import Data.List ( sortOn )
import Data.Ord ( Down(Down) )
import Paths_advent (getDataFileName)

getTotalCalories :: String -> [Integer]
getTotalCalories = fmap countCalories . getInventories 

getInventories :: String -> [[String]]
getInventories = linesBy (=="") <$> lines

countCalories :: [String] -> Integer
countCalories foods = sum $ caloriesIn <$> foods 
    where caloriesIn = read
    
main :: IO ()
main = do
    input <- getDataFileName "day1" >>= readFile
    let calories = sortOn Down . getTotalCalories $ input
    print . head $ calories 
    print . sum $ take 3 calories 
