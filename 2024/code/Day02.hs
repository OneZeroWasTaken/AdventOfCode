{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 where

import           Data.List

main :: IO ()
main = do
  l <- map (map read . words) . lines <$> readFile "inputs/input02.txt"

  putStrLn $ "Part 1: " ++ show (length $ filter safe l)
  putStrLn $ "Part 2: " ++ show (length $ filter (any safe) $ map removeOne l)

removeOne :: [Int] -> [[Int]]
removeOne es = zipWith (++) (inits es) (tail $ tails es)

safe :: [Int] -> Bool
safe is' = cont is' [1, 2, 3] || cont is' [-1, -2, -3]
 where
  cont [_] _ = True
  cont (a : b : is) ds | b - a `elem` ds = cont (b : is) ds
                       | otherwise       = False
