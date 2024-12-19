module Day19 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input19.txt"
  let (ts, ss) = parse l

  putStrLn $ "Part 1: " ++ show (length $ filter (run ts) ss)

run :: [String] -> String -> Bool
run ts s = s `elem` ts || any (\s' -> run ts (drop (length s') s)) (filter (`isPrefixOf` s) ts)

parse :: [String] -> ([String], [String])
parse ss = (splitOn ", " $ head ss, drop 2 ss)
