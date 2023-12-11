module Day11 where

import           Data.List

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input11.txt"
  putStrLn $ "Part 1: " ++ show (calculate ss 2)
  putStrLn $ "Part 2: " ++ show (calculate ss 1000000)

calculate :: [String] -> Int -> Int
calculate ss factor =
  sum . map (distance factor $ noGalaxies ss) . pairs . galaxyPositions $ ss

galaxyPositions :: [String] -> [(Int, Int)]
galaxyPositions ss = filter (\(x, y) -> ss !! y !! x == '#') ps
 where
  ps = [ (x, y) | x <- [0 .. length (head ss) - 1], y <- [0 .. length ss - 1] ]

pairs :: [a] -> [(a, a)]
pairs ls = [ (x, y) | (x : ys) <- tails ls, y <- ys ]

distance :: Int -> ([Int], [Int]) -> ((Int, Int), (Int, Int)) -> Int
distance factor (ex, ey) (p1@(x1, y1), p2@(x2, y2)) = manhattan p1 p2 + nx + ny
 where
  nx = (factor - 1) * length ([min x1 x2 .. max x1 x2] `intersect` ex)
  ny = (factor - 1) * length ([min y1 y2 .. max y1 y2] `intersect` ey)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

noGalaxies :: [String] -> ([Int], [Int])
noGalaxies ss = (f $ transpose ss, f ss)
  where f ss = map snd $ filter (all (== '.') . fst) (zip ss [0 ..])

