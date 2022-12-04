module Day2 where

import           Data.List

main :: IO ()
main = do
  content <- readFile "inputs/input2.txt"
  let score1 = calculate parseToRps content
  putStrLn $ "Part 1 score: " ++ show score1
  let score2 = calculate parseToRps2 content
  putStrLn $ "Part 2 score: " ++ show score2
  return ()

calculate :: (String -> (RPS, RPS)) -> String -> Int
calculate parse = sum . map play . map parse . lines

parseToRps :: String -> (RPS, RPS)
parseToRps s = (charToRps $ head s, charToRps $ last s)

parseToRps2 :: String -> (RPS, RPS)
parseToRps2 s = (opponent, charToRps2 (last s) opponent)
  where opponent = charToRps $ head s


data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

instance Ord RPS where
  Rock     <= Paper    = True
  Paper    <= Scissors = True
  Scissors <= Rock     = True
  Rock     <= Scissors = False
  Scissors <= Paper    = False
  Paper    <= Rock     = False

play :: (RPS, RPS) -> Int
play (o, y) | y > o     = 6 + score y
            | y == o    = 3 + score y
            | otherwise = 0 + score y

score :: RPS -> Int
score Rock     = 1
score Paper    = 2
score Scissors = 3

charToRps2 :: Char -> RPS -> RPS
charToRps2 'X' Rock     = Scissors
charToRps2 'X' Paper    = Rock
charToRps2 'X' Scissors = Paper
charToRps2 'Y' d        = d
charToRps2 'Z' Rock     = Paper
charToRps2 'Z' Paper    = Scissors
charToRps2 'Z' Scissors = Rock
charToRps2 _   _        = error "Cannot parse"


charToRps :: Char -> RPS
charToRps 'A' = Rock
charToRps 'B' = Paper
charToRps 'C' = Scissors
charToRps 'X' = Rock
charToRps 'Y' = Paper
charToRps 'Z' = Scissors
charToRps _   = error "Cannot parse"

