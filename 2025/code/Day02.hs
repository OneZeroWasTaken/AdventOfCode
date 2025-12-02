module Day02 where

import Data.List.Split
import Data.List

main :: IO ()
main = do
  l <- splitOn "," <$> readFile "inputs/input02.txt"

  putStrLn $ "Part 1: " ++ show (sum $ filter isInvalid1 $ concatMap toRange l)
  putStrLn $ "Part 2: " ++ show (sum $ filter isInvalid2 $ concatMap toRange l)

isInvalid1 :: Int -> Bool
isInvalid1 n = isInvalid (show n) (length (show n) `div` 2)

isInvalid2 :: Int -> Bool
isInvalid2 n = isInvalid (show n) 1

isInvalid :: String -> Int -> Bool
isInvalid s n
   | n > length s `div` 2 = False
   | length (last a) /= n = False
   | otherwise = (length s == sum (map length a) && all (== head a) a) || isInvalid s (n + 1)
  where
   a = divvy n n s

toRange :: String -> [Int]
toRange s =
  let [start, end] = splitOn "-" s
   in [read start .. read end]
