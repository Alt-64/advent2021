module Main where

import Data.List.Split(wordsBy)

getAssignments :: String -> [[Integer]]
getAssignments line = (read <$>) <$> (wordsBy (=='-')) <$> (wordsBy (==',')) line

hasFullOverlap :: [[Integer]] -> Bool
hasFullOverlap xs = 
    let s1 = head $ head xs
        e1 = last $ head xs
        s2 = head $ last xs
        e2 = last $ last xs 
    in ((s1 <= s2) && (e1 >= e2)) || ((s1 >= s2) && (e1 <= e2))
    
hasAnyOverlap :: [[Integer]] -> Bool
hasAnyOverlap xs = 
    let s1 = head $ head xs
        e1 = last $ head xs
        s2 = head $ last xs
        e2 = last $ last xs 
    in ((s1 <= e2) && (s2 <= e1)) 


main :: IO ()
main = do
    input <- getContents
    let assignments = getAssignments <$> lines input
    putStrLn . show . length $ (filter hasFullOverlap assignments)
    putStrLn . show . length $ (filter hasAnyOverlap assignments)
    
