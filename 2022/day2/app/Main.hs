module Main where

import Data.List.Split

getRound :: String -> (Char, Char)
getRound str = (head str, last str)

getRounds :: String -> [(Char, Char)]
getRounds input = getRound <$> lines input

scorePart1 :: (Char, Char) -> Integer
scorePart1 x = case x of
    ('A', 'X') -> 3 + 1
    ('A', 'Y') -> 6 + 2
    ('A', 'Z') -> 0 + 3
    ('B', 'X') -> 0 + 1
    ('B', 'Y') -> 3 + 2
    ('B', 'Z') -> 6 + 3
    ('C', 'X') -> 6 + 1
    ('C', 'Y') -> 0 + 2
    ('C', 'Z') -> 3 + 3
    _ -> 0

scorePart2 :: (Char, Char) -> Integer
scorePart2 x = case x of
    ('A', 'X') -> 0 + 3
    ('A', 'Y') -> 3 + 1
    ('A', 'Z') -> 6 + 2
    ('B', 'X') -> 0 + 1
    ('B', 'Y') -> 3 + 2
    ('B', 'Z') -> 6 + 3
    ('C', 'X') -> 0 + 2
    ('C', 'Y') -> 3 + 3
    ('C', 'Z') -> 6 + 1
    _ -> 0

main :: IO ()
main = do
    rounds <- getRounds <$> getContents
    putStrLn . show . sum $ scorePart1 <$> rounds
    putStrLn . show . sum $ scorePart2 <$> rounds
