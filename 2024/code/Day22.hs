module Day22 where

import           Data.Bits

main :: IO ()
main = do
  is <- map read . lines <$> readFile "inputs/input22.txt"

  putStrLn $ "Part 1: " ++ show (sum $ map ((!! 2000) . iterate secret) is)
  putStrLn $ "Part 2: " ++ show ()

secret :: Int -> Int
secret s = prune $ mix (n2 * 2048) n2
 where
  n1 = prune $ mix (s * 64) s
  n2 = prune $ mix (n1 `div` 32) n1

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)
