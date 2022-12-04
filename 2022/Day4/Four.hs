module Four where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (calculate contained l)
  putStrLn $ "Part 2: " ++ show (calculate overlaps l)
  return ()

calculate :: (([Int], [Int]) -> Bool) -> [String] -> Int
calculate f = countTrues . map (f . parseRange)

parseRange :: String -> ([Int], [Int])
parseRange s = ([a .. b], [c .. d])
  where (a : b : c : d : []) = read <$> splitOneOf ",-" s

contained :: ([Int], [Int]) -> Bool
contained (ab, cd) = length (union ab cd) `elem` [length ab, length cd]

overlaps :: ([Int], [Int]) -> Bool
overlaps (ab, cd) = length (union ab cd) /= length ab + length cd

countTrues :: [Bool] -> Int
countTrues = length . filter (\b -> b)

