module TwentyOne where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  --content <- readFile "input.txt"
  partOne
  putStrLn ""
  --partTwo
  return ()

data Player = Player Position Score
 deriving (Show, Eq)

type Position = Int
type Score = Int

player1start :: Position
player1start = 9

player2start :: Position
player2start = 4

dice :: Int -> Int
dice 100 = 1
dice n   = n + 1

dice3 :: Int -> (Int, Position)
dice3 n = (t, f + s + t)
 where
  f = dice n
  s = dice f
  t = dice s

player :: Position -> Player
player p = Player p 0

hasWon :: Player -> Bool
hasWon (Player _ score) = score >= 1000

play :: Player -> Player -> Int -> Int -> (Int, Player, Player)
play current other prev n
  | hasWon other = (n * 3, current, other)
  | otherwise    = play nextCurrent nextOther nextDice (n + 1)
  where (nextDice, nextCurrent, nextOther) = turn prev current other

turn :: Int -> Player -> Player -> (Int, Player, Player)
turn prev current other = (nextDice, other, movePlayer current moves)
  where (nextDice, moves) = dice3 prev

movePlayer :: Player -> Position -> Player
movePlayer (Player pos score) moves = Player square (score + square)
 where
  added  = (pos + moves) `mod` 10
  square = if added == 0 then 10 else added

partOne :: IO ()
partOne = do
  let p1                       = player player1start
      p2                       = player player2start
      (n, (Player _ score), _) = play p1 p2 100 0
  putStrLn $ "Part 1 Score = " ++ show (n * score)
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Distinct Velocities = " ++ show ()
  return ()
