{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day20 where

import Algorithm.Search
import Data.Maybe

type Pos = (Int, Int)

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input20.txt"

  let path = zip (reverse $ dijk ss) [0 ..]

  putStrLn $ "Part 1: " ++ show (length . allSaved 2 $ path)
  putStrLn $ "Part 2: " ++ show (length . allSaved 20 $ path)

allSaved :: Int -> [(Pos, Int)] -> [Int]
allSaved d path = concatMap (filter (>= 100) . tryCheat d path) path

tryCheat :: Int -> [(Pos, Int)] -> (Pos, Int) -> [Int]
tryCheat d path (p, n) = filter (> 0) $ map (\p' -> savedPico (manhattan p p') n $ fromJust $ lookup p' path) cheats
  where
    cheats = filter ((<= d) . manhattan p) $ map fst path

manhattan :: Pos -> Pos -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

savedPico :: Int -> Int -> Int -> Int
savedPico d n m
  | n < m = 0
  | otherwise = n - m - d

dijk :: [String] -> [Pos]
dijk ss = (s :) . snd . fromJust . dijkstra (filter (`elem` path) . neighbors) (\_ _ -> 1) (== e) $ s
  where
    path@(s : e : _) = findCoords 'S' ss ++ findCoords 'E' ss ++ findCoords '.' ss

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findCoords :: (Eq a) => a -> [[a]] -> [Pos]
findCoords i = map fst . concatMap (filter ((== i) . snd) . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)) . zip [0 ..]
