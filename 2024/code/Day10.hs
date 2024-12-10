module Day10 where

import Data.Char
import Data.List

main :: IO ()
main = do
  m <- map (map digitToInt) . lines <$> readFile "inputs/input10.txt"

  putStrLn $ "Part 1: " ++ show (length $ concatMap (nub . trailheadScore m . return) (findCoords 0 m))
  putStrLn $ "Part 2: " ++ show (length $ concatMap (trailheadScore m . return) (findCoords 0 m))

trailheadScore :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
trailheadScore _ [] = []
trailheadScore m ps
  | next == 10 = ps
  | otherwise = trailheadScore m $ concatMap (filter ((== next) . (m @.)) . filter inBounds . neighbors) ps
  where
    inBounds (x', y') = x' >= 0 && y' >= 0 && x' < length m && y' < length (head m)
    next = m @. head ps + 1

(@.) :: [[a]] -> (Int, Int) -> a
m @. (x, y) = m !! y !! x

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findCoords :: (Eq a) => a -> [[a]] -> [(Int, Int)]
findCoords i =
  map fst . concatMap (filter ((== i) . snd) . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)) . zip [0 ..]
