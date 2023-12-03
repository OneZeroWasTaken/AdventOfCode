module Day03 where

import           Data.List
import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input03.txt"

  let part1 = concatMap (numbersAdjacentToCoord l) (coordsOf symbols l)
  putStrLn $ "Part 1: " ++ show (sum part1)

  let gearNums = map (numbersAdjacentToCoord l) (coordsOf gears l)
      part2 = map (\[a, b] -> a * b) $ filter (\ns -> length ns == 2) gearNums
  putStrLn $ "Part 2: " ++ show (sum part2)

gears :: Char -> Bool
gears = (== '*')

symbols :: Char -> Bool
symbols c = not (isDigit c || c == '.')

coordsOf :: (Char -> Bool) -> [String] -> [(Int, Int)]
coordsOf f ss = concatMap coordsOf' (zip3 ss (repeat 0) [0 ..])
 where
  coordsOf' ([], _, _) = []
  coordsOf' (c : cs, x, y) | f c       = (x, y) : coordsOf' (cs, x + 1, y)
                           | otherwise = coordsOf' (cs, x + 1, y)

numbersAdjacentToCoord :: [String] -> (Int, Int) -> [Int]
numbersAdjacentToCoord ss = nub . filter (/= 0) . map (numberAt ss) . deltas

deltas :: (Int, Int) -> [(Int, Int)]
deltas (sx, sy) = [ (x, y) | x <- [sx - 1 .. sx + 1], y <- [sy - 1 .. sy + 1] ]

numberAt :: [String] -> (Int, Int) -> Int
numberAt ss (x, y)
  | y < 0 || y >= length ss        = 0
  | x < 0 || x >= length (ss !! y) = 0
  | (not . isDigit) c              = 0
  | otherwise = read $ reverse (takeWhile isDigit l) ++ takeWhile isDigit r
 where
  c = ss !! y !! x
  l = reverse $ take x (ss !! y)
  r = drop x (ss !! y)
