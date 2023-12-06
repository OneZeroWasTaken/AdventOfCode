module Day06 where

import           Data.List.Split

main :: IO ()
main = do
  [time, dist] <- lines <$> readFile "inputs/input06.txt"

  let part1 = withKerning time dist
  let part2 = withoutKerning time dist
  putStrLn $ "Part 1: " ++ show (product $ map bruteForce part1)
  putStrLn $ "Part 2: " ++ show (bruteForce part2)

bruteForce :: (Int, Int) -> Int
bruteForce (t, d) = winCount d (distancesFromTime t 0)

winCount :: Int -> [Int] -> Int
winCount w = length . filter (> w)

distancesFromTime :: Int -> Int -> [Int]
distancesFromTime 0 s = [0]
distancesFromTime t s = t * s : distancesFromTime (t - 1) (s + 1)

withKerning :: String -> String -> [(Int, Int)]
withKerning time dist = zip (map read . parse $ time) (map read . parse $ dist)

withoutKerning :: String -> String -> (Int, Int)
withoutKerning time dist =
  (read . concat . parse $ time, read . concat . parse $ dist)

parse :: String -> [String]
parse = filter (not . null) . splitOneOf "Time: Dstanc"

