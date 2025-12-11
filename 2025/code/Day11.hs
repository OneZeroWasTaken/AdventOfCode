module Day11 where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input11.txt"

  putStrLn $ "Part 1: " ++ show (countPaths "you" "out" (next $ map parse l))
  putStrLn $ "Part 2: " ++ show ()

countPaths :: String -> String -> (String -> [String]) -> Int
countPaths start goal next
  | start == goal = 1
  | otherwise = sum [countPaths n goal next | n <- next start]

next :: [(String, [String])] -> String -> [String]
next sts s
  | s == "out" = []
  | otherwise = fromJust $ lookup s sts

parse :: String -> (String, [String])
parse s = (a, as)
  where
    (a : as) = filter (not . null) $ splitOneOf ": " s
