module Day06 where

import Data.List.Split
import Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input06.txt"

  putStrLn $ "Part 1: " ++ show (sum $ map calc $ transpose $ map words l)
  putStrLn $ "Part 2: " ++ show (sum $ map calc (splitOn ["     "] $ transpose l))

reead :: [String] -> [Int]
reead = map read . filter (not . null) . map (filter (`elem` ['0'..'9']))

op :: [String] -> (Int -> Int -> Int)
op s | head (last s) == '+' = (+)
     | head (last s) == '*' = (*)
     | otherwise = op $ reverse (map reverse s)

calc :: [String] -> Int
calc s = foldl1 (op s) $ reead s
