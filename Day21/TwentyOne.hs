module TwentyOne where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

data Player = Player Int Position Score
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

player :: Int -> Position -> Player
player i p = Player i p 0

playerId :: Player -> Int
playerId (Player i _ _) = i

hasWon2 :: Player -> Bool
hasWon2 (Player _ _ score) = score >= 21

hasWon :: Player -> Bool
hasWon (Player _ _ score) = score >= 1000

play2 :: Player -> Player -> Int -> (Integer, Integer)
play2 current nextCurrent n
  | hasWon2 nextCurrent = case playerId nextCurrent of
    0 -> (1, 0)
    1 -> (0, 1)
    _ -> error "No player"
  | otherwise = wins''
 where
  nextOther = movePlayer current n
  wins      = map (play2 nextCurrent nextOther) [3 .. 9]
  wins'     = zipWith (\(k, m) i -> (k * i, m * i)) wins [1, 3, 6, 7, 6, 3, 1]
  wins''    = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) wins'

play :: Player -> Player -> Int -> Int -> (Int, Player, Player)
play current other prev n
  | hasWon other = (n * 3, current, other)
  | otherwise    = play nextCurrent nextOther nextDice (n + 1)
  where (nextDice, nextCurrent, nextOther) = turn prev current other

turn :: Int -> Player -> Player -> (Int, Player, Player)
turn prev current other = (nextDice, other, movePlayer current moves)
  where (nextDice, moves) = dice3 prev

movePlayer :: Player -> Position -> Player
movePlayer (Player i pos score) moves = Player i square (score + square)
 where
  added  = (pos + moves) `mod` 10
  square = if added == 0 then 10 else added

partOne :: IO ()
partOne = do
  let p1                         = player 0 player1start
      p2                         = player 1 player2start
      (n, (Player _ _ score), _) = play p1 p2 100 0
  putStrLn $ "Part 1 Score = " ++ show (n * score)
  return ()

partTwo :: IO ()
partTwo = do
  let p1 = player 0 player1start
      p2 = player 1 player2start
      g1 = play2 p1 p2 1
      g2 = play2 p1 p2 2
      g3 = play2 p1 p2 3
      s0 = sum $ map fst [g1, g2, g3]
      s1 = sum $ map snd [g1, g2, g3]
  putStrLn $ "Part 2 Wins = " ++ show (s0, s1)
  return ()
