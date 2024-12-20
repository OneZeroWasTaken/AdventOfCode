{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day20 where

import Algorithm.Search
import Data.Maybe

type Pos = (Int, Int)

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input20.txt"

  let a = zip (reverse $ dijk ss) [0 ..]

  putStrLn $ "Part 1: " ++ show (length . concatMap (filter (>= 100) . tryCheat a) $ a)
  putStrLn $ "Part 2: " ++ show (length . concatMap (filter (>= 100) . tryCheatBig a) $ a)

tryCheatBig :: [(Pos, Int)] -> (Pos, Int) -> [Int]
tryCheatBig path (p, n) = cheats
  where
    ps = map fst path
    potCheats = filter ((<= 20) . manhattan p) ps
    cheats = filter (> 0) $ map (\p' -> savedPico (manhattan p p') n $ fromJust $ lookup p' path) potCheats

manhattan :: Pos -> Pos -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

tryCheat :: [(Pos, Int)] -> (Pos, Int) -> [Int]
tryCheat path (p, n) = filter (> 0) $ map (\p' -> savedPico 2 n $ fromJust $ lookup p' path) cheats
  where
    ps = map fst path
    cheats = map fst $ filter (\(p', q) -> p' `elem` ps && q `notElem` ps) $ zip (neighborsDelta 2 p) (neighbors p)

savedPico :: Int -> Int -> Int -> Int
savedPico d n m
  | n < m = 0
  | otherwise = n - m - d

dijk :: [String] -> [Pos]
dijk ss = (s :) . snd . fromJust . dijkstra (filter (`elem` path) . neighbors) (\_ _ -> 1) (== e) $ s
  where
    path@(s : e : _) = findCoords 'S' ss ++ findCoords 'E' ss ++ findCoords '.' ss

neighbors :: Pos -> [Pos]
neighbors = neighborsDelta 1

neighborsDelta :: Int -> Pos -> [Pos]
neighborsDelta d (x, y) = [(x + d, y), (x - d, y), (x, y + d), (x, y - d)]

findCoords :: (Eq a) => a -> [[a]] -> [Pos]
findCoords i = map fst . concatMap (filter ((== i) . snd) . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)) . zip [0 ..]
