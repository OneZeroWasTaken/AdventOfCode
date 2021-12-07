module Seven where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = parseInput content
  partOne input
  putStrLn ""
  partTwo input
  return ()

parseInput :: String -> [Int]
parseInput i = map read (splitOn "," i)


partOne :: [Int] -> IO ()
partOne l = do
  putStrLn $ "Min fuel = " ++ show (minimum' $ fuelSpan l fuel)
  return ()

partTwo :: [Int] -> IO ()
partTwo l = do
  putStrLn $ "Min fuel 2 = " ++ show (minimum' $ fuelSpan l fuel2)
  return ()


fuelSpan :: [Int] -> ([Int] -> Int -> Int) -> [Int]
fuelSpan i f = [ f i n | n <- [0 .. maximum' i] ]


fuel2 :: [Int] -> Int -> Int
fuel2 hs x = sum $ map (\h -> sum [1 .. abs (h - x)]) hs

fuel :: [Int] -> Int -> Int
fuel hs x = sum $ map (\h -> abs $ h - x) hs


minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y -> if x <= y then x else y)

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y -> if x >= y then x else y)


