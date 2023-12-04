{-# LANGUAGE TupleSections #-}
module Day04 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input04.txt"

  let w = map (winning . parse) l
  putStrLn $ "Part 1: " ++ show (points1 w)
  putStrLn $ "Part 2: " ++ show (points2 $ map (, 1) w)

points1 :: [Int] -> Int
points1 [] = 0
points1 (n : ns) | n == 0    = points1 ns
                 | otherwise = points1 ns + 2 ^ (n - 1)

points2 :: [(Int, Int)] -> Int
points2 []              = 0
points2 ((sc, n) : scs) = n + points2 (add sc n scs)

add :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
add c n scs = map (\(a, b) -> (a, b + n)) (take c scs) ++ drop c scs

winning :: ([Int], [Int]) -> Int
winning (a, b) = length $ a `intersect` b

parse :: String -> ([Int], [Int])
parse s = (winNrs, haveNrs)
 where
  [winNrs, haveNrs] =
    map (map read . filter (not . null) . splitOn " ")
      . splitOn "|"
      . tail
      . dropWhile (/= ':')
      $ s
