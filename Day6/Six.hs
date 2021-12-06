module Six where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input  = parseInput content
  let input' = parseInput' content
  let fish   = simulate' input' 256
  putStrLn $ "Part 1 fish = " ++ show (length $ simulate input 80)
  putStrLn ""
  putStrLn $ "Part 2 fish = " ++ show (sum fish)
  return ()

-- | The fast variant
simulate' :: [Int] -> Int -> [Int]
simulate' f 0 = f
simulate' f g = simulate' (simGeneration' f) (g - 1)

simGeneration' :: [Int] -> [Int]
simGeneration' (z : f) = take 6 f ++ [f !! 6 + z, f !! 7, z]
simGeneration' e       = error "Bad input " ++ e

parseInput' :: String -> [Int]
parseInput' i = [ count n i' | n <- [0 .. 8] ]
 where
  i' = parseInput i
  count _ [] = 0
  count n' (e : es) | e == n'   = count n' es + 1
                    | otherwise = count n' es


-- | The slow variant
simulate :: [Int] -> Int -> [Int]
simulate f 0 = f
simulate f g = simulate (simGeneration f) (g - 1)

simGeneration :: [Int] -> [Int]
simGeneration []       = []
simGeneration (0 : fs) = 8 : 6 : simGeneration fs
simGeneration (f : fs) = (f - 1) : simGeneration fs

parseInput :: String -> [Int]
parseInput i = map read (splitOn "," i)


