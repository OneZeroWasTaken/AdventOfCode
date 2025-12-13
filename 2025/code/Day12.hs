module Day12 where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input12.txt"

  let parsed = map parse (drop 30 l)
  putStrLn $ "Part 1: " ++ show (length $ filter fits parsed)

c :: [Int]
c = [7, 7, 6, 7, 5, 7]

fits :: ((Int, Int), [Int]) -> Bool
fits ((w, h), ns) = w * h >= sum (zipWith (*) c ns)

parse :: String -> ((Int, Int), [Int])
parse s = let (a:b:cs) = map read $ filter (not . null) $ splitOneOf "x: " s in ((a, b), cs)
