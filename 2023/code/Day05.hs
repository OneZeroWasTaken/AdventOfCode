module Day05 where

import           Data.List.Split
import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input05.txt"

  let
    (seeds, allMaps) = parse l
    maps = map (foldl1 merge) allMaps
    seedRanges2 = map (\[s1, s2] -> Range s1 $ s1 + s2 - 1) (chunksOf 2 seeds)

  let part1 = minimum $ map (\n -> runValue forward n (reverse maps)) seeds
      part2 = divideAndConquer seedRanges2 maps (0, head seeds)
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

data Map = Map (Int -> Int) (Int -> Int)

divideAndConquer :: [Range] -> [Map] -> (Int, Int) -> Int
divideAndConquer rs ms (lo, hi)
  | hi == lo + 1         = hi
  | any (v `inRange`) rs = divideAndConquer rs ms (lo, mid)
  | otherwise            = divideAndConquer rs ms (mid, hi)
 where
  mid = lo + (hi - lo) `div` 2
  v   = runValue backward mid ms

forward :: Map -> Int -> Int
forward (Map f _) = f

backward :: Map -> Int -> Int
backward (Map _ b) = b

runValue :: (Map -> Int -> Int) -> Int -> [Map] -> Int
runValue = foldr

merge :: Map -> Map -> Map
merge (Map f b) (Map g h) = Map fg bh
 where
  fg n | y == n    = g n
       | otherwise = y
    where y = f n
  bh n | y == n    = h n
       | otherwise = y
    where y = b n

mapOf :: Int -> Int -> Int -> Map
mapOf de so ra = Map
  (\n -> if n >= so && n < so + ra then n - so + de else n)
  (\n -> if n >= de && n < de + ra then n + so - de else n)

data Range = Range Int Int
  deriving (Eq, Show)

inRange :: Int -> Range -> Bool
inRange n (Range b t) = n >= b && n <= t

parse :: [String] -> ([Int], [[Map]])
parse l = (seeds, maps)
 where
  seeds = (map read . filter (not . null) . splitOneOf "seds: " . head) l
  maps  = parseMaps (drop 3 l) []

parseMaps :: [String] -> [Map] -> [[Map]]
parseMaps []        ms = [ms]
parseMaps ([] : ss) ms = parseMaps ss ms
parseMaps (s : ss) ms | (isDigit . head) s = parseMaps ss (mapOf de so ra : ms)
                      | otherwise          = ms : parseMaps ss []
  where [de, so, ra] = map read $ splitOn " " s
