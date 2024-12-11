{-# LANGUAGE TupleSections #-}

module Day11 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  is <- parse . head . lines <$> readFile "inputs/input11.txt"

  putStrLn $ "Part 1: " ++ show (length $ iterate (concatMap stone) is !! 25)
  putStrLn $ "Part 2: " ++ show (sum $ map snd $ iterate run (map (,1) is) !! 75)

run :: [(Int, Int)] -> [(Int, Int)]
run es = nub $ map (\(v, _) -> (v, countV v a)) a
  where
    a = concatMap (\(v, n) -> map (,n) (stone v)) es

countV :: (Eq a) => a -> [(a, Int)] -> Int
countV x = sum . map snd . filter ((x ==) . fst)

stone :: Int -> [Int]
stone 0 = [1]
stone n
  | even e = map read [take mid sn, drop mid sn]
  | otherwise = [n * 2024]
  where
    e = length $ show n
    mid = e `div` 2
    sn = show n

parse :: String -> [Int]
parse = map read . splitOn " "
