module Main where

import Data.List.Split

getRound :: String -> (Char, Char)
getRound str = (head str, last str)

getRounds :: String -> [(Char, Char)]
getRounds input = getRound <$> lns
    where lns = splitOn "\n" input

scorePart1 :: (Char, Char) -> Integer
scorePart1 ('A', 'X') = 3 + 1
scorePart1 ('A', 'Y') = 6 + 2
scorePart1 ('A', 'Z') = 0 + 3
scorePart1 ('B', 'X') = 0 + 1
scorePart1 ('B', 'Y') = 3 + 2
scorePart1 ('B', 'Z') = 6 + 3
scorePart1 ('C', 'X') = 6 + 1
scorePart1 ('C', 'Y') = 0 + 2
scorePart1 ('C', 'Z') = 3 + 3
scorePart1 _ = 0

scorePart2 :: (Char, Char) -> Integer
scorePart2 ('A', 'X') = 0 + 3
scorePart2 ('A', 'Y') = 3 + 1
scorePart2 ('A', 'Z') = 6 + 2
scorePart2 ('B', 'X') = 0 + 1
scorePart2 ('B', 'Y') = 3 + 2
scorePart2 ('B', 'Z') = 6 + 3
scorePart2 ('C', 'X') = 0 + 2
scorePart2 ('C', 'Y') = 3 + 3
scorePart2 ('C', 'Z') = 6 + 1
scorePart2 _ = 0

main :: IO ()
main = do
    rounds <- getRounds <$> getContents
    putStrLn . show . sum $ scorePart1 <$> rounds
    putStrLn . show . sum $ scorePart2 <$> rounds
    return ()
