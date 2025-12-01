module Day01 where

import Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input01.txt"
  putStrLn $ "Part 1: " ++ show (snd . atZero . map parse $ l)
  putStrLn $ "Part 2: " ++ show (snd . atZero . concatMap parse2 $ l)

atZero :: [Int] -> (Int, Int)
atZero ns = foldl next (50, 0) ns
  where
    next (cur, z) n
      | (cur + n) `mod` 100 == 0 = (cur + n, z + 1)
      | otherwise = (cur + n, z)

parse2 :: String -> [Int]
parse2 ('L' : cs) = replicate (read cs) (-1)
parse2 ('R' : cs) = replicate (read cs) 1

parse :: String -> Int
parse ('L' : cs) = -read cs
parse ('R' : cs) = read cs