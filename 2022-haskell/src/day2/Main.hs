module Main where

import Text.Parsec ( char, space, endBy, (<|>), parse, ParsecT )
import Paths_advent (getDataFileName)
import Data.Functor.Identity ( Identity )

data Choice = Rock | Paper | Scissors deriving Eq
data Symbol = X | Y | Z
data Result = Loss | Draw | Win

line :: ParsecT String u Identity (Choice, Symbol)
line = do
    left <-     Rock        <$ char 'A'
            <|> Paper       <$ char 'B'
            <|> Scissors    <$ char 'C'
    space
    right <-    X           <$ char 'X'
            <|> Y           <$ char 'Y'
            <|> Z           <$ char 'Z'
    return (left, right)

input :: ParsecT String u Identity [(Choice, Symbol)]
input = endBy line eol
    where eol = char '\n'

loseTo :: Choice -> Choice 
loseTo x = case x of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper

winTo :: Choice -> Choice 
winTo x = case x of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock
 
playPart1 :: (Choice, Symbol) -> (Result, Choice)
playPart1 (their_choice, our_symbol) = (our_result, our_choice)
    where
        our_choice = case our_symbol of
            X -> Rock
            Y -> Paper
            Z -> Scissors
        our_result
          | our_choice == their_choice = Draw
          | our_choice == winTo their_choice = Win
          | otherwise = Loss

playPart2 :: (Choice, Symbol) -> (Result, Choice)
playPart2 (their_choice, our_symbol) = (our_result, our_choice)
    where
        our_result = case our_symbol of
            X -> Loss
            Y -> Draw
            Z -> Win
        our_choice = case our_result of
            Draw -> their_choice
            Win -> winTo their_choice 
            Loss -> loseTo their_choice

scoreRound :: Num a => (Result, Choice) -> a
scoreRound (result, choice) = result_score + choice_score
    where
        result_score = case result of
            Win -> 6
            Draw -> 3
            Loss -> 0
        choice_score = case choice of
            Rock -> 1
            Paper -> 2
            Scissors -> 3

filename = "input/day2.txt"
main :: IO ()
main = do
    filecontents <- getDataFileName filename >>= readFile 
    case parse input filename filecontents of
        Left err -> print err
        Right xs -> print (soln1, soln2)
            where
                soln1 = sum $ scoreRound . playPart1 <$> xs
                soln2 = sum $ scoreRound . playPart2 <$> xs
    
