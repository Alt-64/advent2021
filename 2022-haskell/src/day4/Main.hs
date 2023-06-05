module Main where

import Data.List.Split(wordsBy)

getAssignments :: String -> [[Integer]]
getAssignments = fmap (fmap read . wordsBy (=='-')) . wordsBy (==',')

getEndpoints :: [[Integer]] -> ((Integer, Integer) , (Integer, Integer))
getEndpoints xs = 
    let s1 = head $ head xs
        e1 = last $ head xs
        s2 = head $ last xs
        e2 = last $ last xs 
    in ((s1, e1), (s2, e2))

hasFullOverlap :: [[Integer]] -> Bool
hasFullOverlap xs = 
    let ((s1, e1), (s2, e2)) = getEndpoints xs
    in ((s1 <= s2) && (e1 >= e2)) || ((s1 >= s2) && (e1 <= e2))
    
hasAnyOverlap :: [[Integer]] -> Bool
hasAnyOverlap xs = 
    let ((s1, e1), (s2, e2)) = getEndpoints xs
    in ((s1 <= e2) && (s2 <= e1)) 


main :: IO ()
main = do
    input <- getContents
    let assignments = getAssignments <$> lines input
    print . length $ filter hasFullOverlap assignments
    print . length $ filter hasAnyOverlap assignments
    
