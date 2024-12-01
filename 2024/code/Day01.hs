{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day01 where

import           Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input01.txt"
  let (a', b') = unzip $ map parse l
      (a , b ) = (sort a', sort b')

  putStrLn $ "Part 1: " ++ show (sum $ zipWith (curry distance) a b)
  putStrLn $ "Part 2: " ++ show (sum $ map (`similarity` b) a)

similarity :: Int -> [Int] -> Int
similarity n = (* n) . length . filter (== n)

distance :: (Int, Int) -> Int
distance (a, b) = abs (a - b)

parse :: String -> (Int, Int)
parse s = (l, r) where [l, r] = map read . words $ s
