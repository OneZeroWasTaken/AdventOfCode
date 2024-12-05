{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day05 where

import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input05.txt"
  let (rules, updates) = parse l []

  putStrLn $ "Part 1: " ++ show (part1 rules updates)
  putStrLn $ "Part 2: " ++ show (part2 rules updates)

correctTheOrder :: [(Int, [Int])] -> ([Int], Int) -> Int
correctTheOrder m (e : es, target) = case lookup e m of
  Nothing -> correctTheOrder m (es ++ [e], target)
  Just a  -> if length (filter (`elem` a) es) == target
    then e
    else correctTheOrder m (es ++ [e], target)

part2 :: [(Int, [Int])] -> [[Int]] -> Int
part2 m u = sum
  $ map (correctTheOrder m . (\a -> (a, length a `div` 2))) incorrect
  where incorrect = filter (not . rightOrder m) u

part1 :: [(Int, [Int])] -> [[Int]] -> Int
part1 m = sum . map midElem . filter (rightOrder m)

midElem :: [a] -> a
midElem es = (!! (length es `div` 2)) es

rightOrder :: [(Int, [Int])] -> [Int] -> Bool
rightOrder _ [_     ] = True
rightOrder m (e : es) = case lookup e m of
  Nothing -> False
  Just a  -> all (`elem` a) es && rightOrder m es

parse :: [String] -> [(Int, [Int])] -> ([(Int, [Int])], [[Int]])
parse ([] : ss) acc = (acc, map (map read . splitOn ",") ss)
parse (s  : ss) acc = case lookup k acc of
  Nothing -> parse ss ((k, [v]) : acc)
  Just v' -> parse ss (updateLookup k (v : v') acc)
  where [k, v] = map read $ splitOn "|" s

updateLookup :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateLookup _ _ [] = []
updateLookup a b (kv@(k, _) : kvs) | a == k    = (k, b) : kvs
                                   | otherwise = kv : updateLookup a b kvs
