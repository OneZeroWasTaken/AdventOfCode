module Day09 where

import           Data.List.Split

main :: IO ()
main = do
  l <- map parse . lines <$> readFile "inputs/input09.txt"
  putStrLn $ "Part 1: " ++ show (sum $ map predictForward l)
  putStrLn $ "Part 2: " ++ show (sum $ map predictBack l)

predictBack :: [Int] -> Int
predictBack = foldr (-) 0 . diffs last . reverse

predictForward :: [Int] -> Int
predictForward = sum . diffs head . reverse

diffs :: ([Int] -> Int) -> [Int] -> [Int]
diffs f ns | all (== 0) ns = [f ns]
           | otherwise     = f ns : diffs f (zipWith (-) ns (tail ns))

parse :: String -> [Int]
parse = map read . splitOn " "
