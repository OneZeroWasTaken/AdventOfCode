{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day18 where

import Algorithm.Search
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input18.txt"

  let obs = map parse l

  putStrLn $ "Part 1: " ++ show (fst . fromJust . dijk $ take 1024 obs)
  putStrLn $ "Part 2: " ++ show (obs !! divAndConq obs (length obs `div` 2) (length obs `div` 2))

divAndConq :: [(Int, Int)] -> Int -> Int -> Int
divAndConq allObs n step
  | path && not (hasPath allObs (n + 1)) = n
  | path = divAndConq allObs (n + 1 + step `div` 2) nStep
  | otherwise = divAndConq allObs (n - 1 - step `div` 2) nStep
  where
    path = hasPath allObs n
    nStep = max 1 (step `div` 2)

hasPath :: [(Int, Int)] -> Int -> Bool
hasPath allObs n = isJust $ dijk (take n allObs)

bounds :: (Int, Int)
bounds = (70, 70)

dijk :: [(Int, Int)] -> Maybe (Int, [(Int, Int)])
dijk obs = dijkstra ((\\ obs) . neighborStates bounds) (\_ _ -> 1) (== bounds) (0, 0)

neighborStates :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighborStates bs p = filter (withinBounds bs) $ neighbors p

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

withinBounds :: (Int, Int) -> (Int, Int) -> Bool
withinBounds (w, h) (x, y) = x >= 0 && x <= w && y >= 0 && y <= h

parse :: String -> (Int, Int)
parse s = let [x, y] = splitOn "," s in (read x, read y)
