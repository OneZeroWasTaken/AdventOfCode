module Day03 where

import Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input03.txt"

  putStrLn $ "Part 1: " ++ show (sum $ map (joltage 1 . parse) l)
  putStrLn $ "Part 2: " ++ show (sum $ map (joltage 11 . parse) l)

merge :: [Int] -> Int
merge = read . concatMap show

joltage :: Int -> [Int] -> Int
joltage 0 bs = maximum bs
joltage n bs = merge $ left : [joltage (n - 1) rest]
  where
    rest = tail . dropWhile (/= left) $ bs
    left = maximum (dropLast n bs)

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

parse :: String -> [Int]
parse = map digitToInt
