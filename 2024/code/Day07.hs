{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day07 where

import           Data.List.Split

main :: IO ()
main = do
  l <- map parse . lines <$> readFile "inputs/input07.txt"

  putStrLn $ "Part 1: " ++ (show . run [(+), (*)]) l
  putStrLn $ "Part 2: " ++ (show . run [(+), (*), (-||-)]) l

run :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
run ops = sum . map fst . filter (uncurry $ eqEvaluates 0 ops)

eqEvaluates :: Int -> [Int -> Int -> Int] -> Int -> [Int] -> Bool
eqEvaluates acc _   val []       = val == acc
eqEvaluates 0   ops val (i : is) = eqEvaluates i ops val is
eqEvaluates acc ops val (i : is) =
  any (\op -> eqEvaluates (acc `op` i) ops val is) ops

(-||-) :: Int -> Int -> Int
x -||- y = read $ show x ++ show y

parse :: String -> (Int, [Int])
parse s = let (val : _ : is) = map read $ splitOneOf ": " s in (val, is)
