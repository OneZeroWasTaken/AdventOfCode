module Day05 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  [rs', ns'] <- map lines . (splitOn "\n\n") <$> readFile "inputs/input05.txt"
  let (rs, ns) = parse (rs', ns')

  putStrLn $ "Part 1: " ++ show (length $ filter (spoiled rs) ns)
  putStrLn $ "Part 2: " ++ show (sum $ map (uncurry rangeSize) $ calcRanges [] rs)

calcRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
calcRanges crs [] = crs
calcRanges [] (r : rs) = calcRanges [r] rs
calcRanges (cur : crs) (r : rs) = calcRanges (expandRange cur r ++ crs) rs

rangeSize :: Int -> Int -> Int
rangeSize l r = r - l + 1

expandRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
expandRange (a, b) (c, d)
  | a == c && b == d = [(a, b)]
  | c <= b && d <= b = [(a, b)]
  | c <= b && d >= b = [(a, d)]
  | otherwise = [(c, d), (a, b)]

spoiled :: [(Int, Int)] -> Int -> Bool
spoiled rs n = any (inRange n) rs

inRange :: Int -> (Int, Int) -> Bool
inRange n (l, r) = n >= l && n <= r

parse :: ([String], [String]) -> ([(Int, Int)], [Int])
parse (rs, ns) = (sort $ map ((\[l, r] -> (l, r)) . map read . splitOn "-") rs, map read ns)
