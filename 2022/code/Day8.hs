module Day8 where

import           Data.Char
import           Data.List
import           Data.Maybe

size :: (Int, Int)
size = (99, 99)
--size = (5, 5)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input8.txt"
  let grid = map (map digitToInt) l
      ss   = maximum $ map (product . scenicScore grid) coords
  putStrLn $ "Part 1 Visible trees: " ++ show (visible grid)
  putStrLn $ "Part 2 Scenic score:  " ++ show ss
  return ()

scenicScore :: [[Int]] -> (Int, Int) -> [Int]
scenicScore g c@(x, y) =
  map (untilTaller g (g !! y !! x) . coordsFromDir c) dirs

untilTaller :: [[Int]] -> Int -> [(Int, Int)] -> Int
untilTaller g t cs =
  (+ 1) . fromMaybe (length cs - 1) $ findIndex (\(x, y) -> g !! y !! x >= t) cs

visible :: [[Int]] -> Int
visible g = length (filter (not . isHidden g) coords) + outerCount

isHidden :: [[Int]] -> (Int, Int) -> Bool
isHidden g c@(x, y) = all ((>= g !! y !! x) . tallest g . coordsFromDir c) dirs

data Dir = L | R | U | D

dirs :: [Dir]
dirs = [L, R, U, D]

coordsFromDir :: (Int, Int) -> Dir -> [(Int, Int)]
coordsFromDir (sx, sy) L = reverse [ (x, sy) | x <- [0 .. sx - 1] ]
coordsFromDir (sx, sy) R = [ (x, sy) | x <- [sx + 1 .. fst size - 1] ]
coordsFromDir (sx, sy) U = reverse [ (sx, y) | y <- [0 .. sy - 1] ]
coordsFromDir (sx, sy) D = [ (sx, y) | y <- [sy + 1 .. snd size - 1] ]

tallest :: [[Int]] -> [(Int, Int)] -> Int
tallest g = maximum . map (\(x, y) -> g !! y !! x)

coords :: [(Int, Int)]
coords = [ (x, y) | x <- [1 .. fst size - 2], y <- [1 .. snd size - 2] ]

outerCount :: Int
outerCount = 2 * uncurry (+) size - 4
