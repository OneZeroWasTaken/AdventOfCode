module Day04 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input04.txt"

  putStrLn $ "Part 1: " ++ show (checkAllDir l)
  putStrLn $ "Part 2: " ++ show (allMas l)

allMas :: [String] -> Int
allMas ss = sum [e, w, s, n]
  where
    pos = [(x, y) | x <- [1 .. length (head ss) - 2], y <- [1 .. length ss - 2]]
    e = mas ss pos
    w = mas (map reverse ss) pos
    s = mas (map reverse $ transpose ss) pos
    n = mas (transpose $ map reverse ss) pos

mas :: [String] -> [(Int, Int)] -> Int
mas _ [] = 0
mas ss ((x, y) : xys)
  | ss !! y !! x == 'A'
      && ss !! (y - 1) !! (x - 1) == 'M'
      && ss !! (y - 1) !! (x + 1) == 'S'
      && ss !! (y + 1) !! (x - 1) == 'M'
      && ss !! (y + 1) !! (x + 1) == 'S' =
      1 + mas ss xys
  | otherwise = mas ss xys

xmasInRows :: [String] -> Int
xmasInRows = sum . map (length . filter (== "XMAS") . divvy 4 1)

checkAllDir :: [String] -> Int
checkAllDir ss = sum [e, w, s, n, se, sw, nw, ne]
  where
    e = xmasInRows ss
    w = xmasInRows (map reverse ss)
    s = xmasInRows (map reverse $ transpose ss)
    n = xmasInRows (transpose $ map reverse ss)
    size = length ss - 4
    se = diagonal ss (1, 1) [(x, y) | x <- [0 .. size], y <- [0 .. size]]
    sw = diagonal ss (-1, 1) [(x, y) | x <- [3 .. length ss - 1], y <- [0 .. size]]
    nw = diagonal ss (-1, -1) [(x, y) | x <- [3 .. length ss - 1], y <- [3 .. length ss - 1]]
    ne = diagonal ss (1, -1) [(x, y) | x <- [0 .. size], y <- [3 .. length ss - 1]]

diagonal :: [String] -> (Int, Int) -> [(Int, Int)] -> Int
diagonal _ _ [] = 0
diagonal ss (dx, dy) ((x, y) : xys)
  | xmas (x, y) "" = 1 + diagonal ss (dx, dy) xys
  | otherwise = diagonal ss (dx, dy) xys
  where
    xmas :: (Int, Int) -> String -> Bool
    xmas _ "SAMX" = True
    xmas _ [_, _, _, _] = False
    xmas (x', y') s = xmas (x' + dx, y' + dy) (ss !! y' !! x' : s)
