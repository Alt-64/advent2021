module Main where

import Data.List.Split
import Data.List
import Data.Char(isLower, isUpper, toLower, toUpper, ord)

getCompartments :: String -> [String]
getCompartments inventory = chunksOf (div (length inventory) 2) inventory

getCommonItem :: [String] -> Char
getCommonItem = head . foldr1 intersect

getPriority :: Char -> Int
getPriority x | isUpper x = ord x - ord 'A' + 27
              | isLower x = ord x - ord 'a' + 1
              | otherwise = 0 

main :: IO ()
main = do
    input <- getContents
    let inventories = lines input
    putStrLn . show . sum $ getPriority . getCommonItem . getCompartments <$> inventories
    putStrLn . show . sum $ getPriority . getCommonItem <$> chunksOf 3 inventories
