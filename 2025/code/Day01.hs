module Day01 where

import           Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input01.txt"
  putStrLn $ "Part 1: " ++ show (snd . atZero . map parse $ l)

atZero :: [Int] -> (Int, Int)
atZero ns = foldl (\(cur, z) n -> next cur z n) (50, 0) ns
  where
    next cur z n | (cur + n) `mod` 100 == 0 = (cur + n, z + 1)
                 | otherwise = (cur + n, z)

parse :: String -> Int
parse ('L':cs) = - read cs
parse ('R':cs) = read cs