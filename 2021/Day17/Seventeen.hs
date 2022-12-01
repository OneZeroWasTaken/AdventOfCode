module Seventeen where

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
  partTwo
  return ()

highestY :: (Int, Int) -> Int
highestY (y1, y2) = sum [1 .. max (abs y1) (abs y2) - 1]

targetX :: (Int, Int)
targetX = (138, 184)

targetY :: (Int, Int)
targetY = (-125, -71)

simulateAll :: [(Int, Int)] -> Int
simulateAll [] = 0
simulateAll (v : vs) | simulate (0, 0) v = simulateAll vs + 1
                     | otherwise         = simulateAll vs

simulate :: (Int, Int) -> (Int, Int) -> Bool
simulate (px, py) (vx, vy)
  | pastTarget (px, py) = False
  | inTarget (px, py)   = True
  | otherwise           = simulate (px + vx, py + vy) (max (vx - 1) 0, vy - 1)

velocities :: [(Int, Int)]
velocities =
  [ (x, y) | x <- [0 .. snd targetX], y <- [fst targetY .. abs (fst targetY)] ]

pastTarget :: (Int, Int) -> Bool
pastTarget (x, y) = x > snd targetX || y < fst targetY

inTarget :: (Int, Int) -> Bool
inTarget (x, y) = inRange x targetX && inRange y targetY

inRange :: Int -> (Int, Int) -> Bool
inRange i (a, b) = i >= a && i <= b

partOne :: IO ()
partOne = do
  putStrLn $ "Part 1 Highest Y = " ++ show (highestY targetY)
  return ()

partTwo :: IO ()
partTwo = do
  let count = simulateAll velocities
  putStrLn $ "Part 2 Distinct Velocities = " ++ show count
  return ()

