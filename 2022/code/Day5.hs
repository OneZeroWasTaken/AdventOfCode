module Day5 where

import           Data.List
import           Data.List.Split
import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input5.txt"
  let (c, m)  = parse l
  let top9000 = map head $ tail $ foldl (move reverse) c m
  let top9001 = map head $ tail $ foldl (move id) c m
  putStrLn $ "Part 1: " ++ show top9000
  putStrLn $ "Part 2: " ++ show top9001
  return ()

parse :: [String] -> ([[Char]], [[Int]])
parse ss = ([] : parseContainers (take 8 ss), map parseMoves $ drop 10 ss)

parseContainers :: [String] -> [[Char]]
parseContainers = filter (/= []) <$> map (filter isAlpha) . transpose

parseMoves :: String -> [Int]
parseMoves s = map read <$> filter (/= []) . splitWhen (not . isDigit) $ s

move :: ([Char] -> [Char]) -> [[Char]] -> [Int] -> [[Char]]
move fun cs [n, f, t] = added
 where
  removed = replaceAt f (drop n $ cs !! f) cs
  added   = replaceAt t (stack ++ (removed !! t)) removed
  stack   = fun $ take n $ cs !! f

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

