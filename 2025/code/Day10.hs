module Day10 where

import           Algorithm.Search
import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input10.txt"

  let ms = map parse l
  putStrLn $ "Part 1: " ++ show (sum $ map (fst . fromJust . dijk) ms)
  let (Manual _ f j) = ms !! 0
  putStrLn $ "Part 2: " ++ show (comb f j)

comb :: [[Bool]] -> [Int] -> Maybe Int
comb f' j' = comb' (sortOn (negate . length . filter id) f') j'
 where
  comb' :: [[Bool]] -> [Int] -> Maybe Int
  comb' _ j | all (== 0) j = Just 0
  comb' []       j         = Nothing
  comb' (f : fs) j         = try j maxPresses
   where
    active     = zip f j
    maxPresses = minimum $ map snd $ filter fst active
    try :: [Int] -> Int -> Maybe Int
    try jj _ | all (== 0) jj = Just 0
    try _  0                 = comb' fs j
    try jj presses           = case comb' fs newJ of
      Just v  -> Just (v + presses)
      Nothing -> try jj (presses - 1)
     where
      newJ =
        map (\(isOn, jolts) -> if isOn then jolts - presses else jolts) active

dijk :: Manual -> Maybe (Int, [[Bool]])
dijk (Manual b f _) = dijkstra next
                               (const . const 1)
                               (== b)
                               (replicate (length b) False)
 where
  next :: [Bool] -> [[Bool]]
  next b' = map (zipWith (/=) b') f

data Manual = Manual [Bool] [[Bool]] [Int]
  deriving (Eq, Show)

parse :: String -> Manual
parse s = Manual lights switches joltage
 where
  (ls : es) = map (init . tail) $ splitOn " " s
  lights    = map (== '#') ls
  switches =
    map (\ns -> map (\i -> i `elem` ns) (take (length ls) [0 ..]))
      $ map (map read . splitOn ",") (init es)
  joltage = (map read . splitOn ",") (last es)
