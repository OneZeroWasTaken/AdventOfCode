module Day9 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input09.txt"

  putStrLn $ "Part 1: " ++ show (maximum $ [area p1 p2 | p1 <- map parse ss, p2 <- map parse ss])
  putStrLn $ "Part 2: " ++ show ()

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

parse :: String -> (Int, Int)
parse s =
  let [x, y] = map read $ splitOn "," s
   in (x, y)
