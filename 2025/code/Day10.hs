module Day10 where

import Algorithm.Search
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input10.txt"

  let ms = map parse l
  putStrLn $ "Part 1: " ++ show (sum $ map (fst . fromJust . dijk) ms)
  putStrLn $ "Part 2: " ++ show (map (fst . fromJust . dijk2) (take 3 ms))

dijk2 :: Manual -> Maybe (Int, [[Int]])
dijk2 (Manual _ f c) = dijkstra next (const . const 1) (== c) (replicate (length c) 0)
  where
    next :: [Int] -> [[Int]]
    next state = filter (all (uncurry (<=)) . zip state) $ map (zipWith up state) f

up :: Int -> Bool -> Int
up n True = n + 1
up n False = n

dijk :: Manual -> Maybe (Int, [[Bool]])
dijk (Manual b f _) = dijkstra next (const . const 1) (== b) (replicate (length b) False)
  where
    next :: [Bool] -> [[Bool]]
    next b' = map (zipWith (/=) b') f

data Manual = Manual [Bool] [[Bool]] [Int]
  deriving (Eq, Show)

parse :: String -> Manual
parse s = Manual lights switches joltage
  where
    (ls : es) = map (init . tail) $ splitOn " " s
    lights = map (== '#') ls
    switches = map (\ns -> map (\i -> i `elem` ns) (take (length ls) [0 ..])) $ map (map read . splitOn ",") (init es)
    joltage = (map read . splitOn ",") (last es)
