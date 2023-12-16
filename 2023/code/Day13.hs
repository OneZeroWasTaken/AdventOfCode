module Day13 where

import           Data.List.Split
import           Data.List

main :: IO ()
main = do
  ss <- splitOn [""] . lines <$> readFile "inputs/input13.txt"

  putStrLn $ "Part 1: " ++ show (sum $ map part1 ss)
  putStrLn $ "Part 2: " ++ show (sum $ map part2 ss)

reflection :: (String -> String -> Bool) -> [String] -> [String] -> [Int]
reflection _ [_] _ = []
reflection f (a : b : c) acc
  | a == b && isReflection  = length (a : acc) : reflection f (b : c) (a : acc)
  | a `f` b && isReflection = length (a : acc) : reflection f (b : c) (a : acc)
  | otherwise               = reflection f (b : c) (a : acc)
  where isReflection = and $ zipWith f (a : c) (a : acc)

(~==~) :: String -> String -> Bool
[] ~==~ [] = True
(s : ss) ~==~ (t : ts) | s == t    = ss ~==~ ts
                       | otherwise = ss == ts

part2 :: [String] -> Int
part2 ss = head $ score (~==~) ss \\ score (==) ss

part1 :: [String] -> Int
part1 ss = head $ score (==) ss

score :: (String -> String -> Bool) -> [String] -> [Int]
score f ss = map (* 100) (reflection f ss []) ++ reflection f (transpose ss) []
