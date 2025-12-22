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
  putStrLn $ "Part 2: " ++ show (sum $ mapMaybe id $ map problem ms)

problem :: Manual -> Maybe Int
problem (Manual _ f j) = solver (map (map fromEnum) f) j

solver :: [[Int]] -> [Int] -> Maybe Int
solver _ j | all (== 0) j = Just 0
           | any (< 0) j  = Nothing
solver f j | null par  = Nothing
           | null c    = Nothing
           | otherwise = Just $ minimum c
 where
  par     = parity f j
  presses = map length par
  a       = map (foldl (zipWith (+)) (replicate (length j) 0)) par
  b       = map (map (`div` 2) . zipWith (-) j . (++ repeat 0)) a
  c       = mapMaybe (\(n, jj) -> (+ n) . (* 2) <$> solver f jj) $ zip presses b

parity :: [[Int]] -> [Int] -> [[[Int]]]
parity f j = filter
  ((== target) . map odd . foldl (zipWith (+)) (replicate (length j) 0))
  (subsequences f)
  where target = map odd j

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
