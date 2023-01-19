module Main where

import Data.List.Split(wordsBy)
import Data.List(transpose)
import Data.Maybe(mapMaybe)
import Data.Char(isAlpha)
import Text.Read(readMaybe)

type Step = (Integer, Integer, Integer)
type Crates = [String] 

readCrates :: [String] -> Crates
readCrates input = (filter (/="")) $ (reverse . filter isAlpha) <$> transpose input

readStep :: String -> Step
readStep line = 
    let xs = mapMaybe readMaybe . words $ line
        count = xs !! 0
        src = xs !! 1
        dst = xs !! 2
    in (count, src, dst)
    
applyStep :: Crates -> Step -> Crates
applyStep step crates = applyStep' step (zip [0..] crates) 
    where applyStep' = case (count, src, dst) (i, cur):



parseInput :: String -> ([String], [String])
parseInput = (splitAt 9) . (filter (/="")) . lines

main :: IO ()
main = do
    input <- getContents
    let (l, r) = parseInput input
    let crates = readCrates l
    mapM (putStrLn . show) $ crates
    let steps = readStep <$> r
    mapM (putStrLn . show) $ steps
    putStrLn "asdf"
    -- mapM (putStrLn . show) assignments
    putStrLn ""
    

