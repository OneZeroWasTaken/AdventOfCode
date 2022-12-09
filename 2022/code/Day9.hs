module Day9 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input9.txt"
  let p     = map parse l
      heads = moveHead s p
  putStrLn $ "Part 1: " ++ (show . length . nub . simRope 1) heads
  putStrLn $ "Part 2: " ++ (show . length . nub . simRope 9) heads
  return ()

simRope :: Int -> [(Int, Int)] -> [(Int, Int)]
simRope 0 cs = cs
simRope n cs = simRope (n - 1) $ move s cs

move :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move _ [] = []
move current@(x, y) (k : ks)
  | touching current k       = current : move current ks
  | x == fst k || y == snd k = (x + ax, y + ay) : move (x + ax, y + ay) ks
  | otherwise                = (x + bx, y + by) : move (x + bx, y + by) ks
 where
  (ax, ay) = findDir xy
  (bx, by) = findDir diags
  findDir :: [(Int, Int)] -> (Int, Int)
  findDir = head . filter (\(dx, dy) -> touching (dx + x, dy + y) k)

diags :: [(Int, Int)]
diags = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

xy :: [(Int, Int)]
xy = [(1, 0), (-1, 0), (0, 1), (0, -1)]

s :: (Int, Int)
s = (-28, 5)

moveHead :: (Int, Int) -> [(Char, Int)] -> [(Int, Int)]
moveHead _ []       = []
moveHead c (d : ms) = cs ++ moveHead (last cs) ms where cs = coordsFromDir c d

touching :: (Int, Int) -> (Int, Int) -> Bool
touching h@(hx, hy) t@(tx, ty) | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = True
                               | otherwise = False

parse :: String -> (Char, Int)
parse s = (head d, read n) where [d, n] = splitOn " " s

coordsFromDir :: (Int, Int) -> (Char, Int) -> [(Int, Int)]
coordsFromDir (sx, sy) ('L', n) = [ (sx - x, sy) | x <- [1 .. n] ]
coordsFromDir (sx, sy) ('R', n) = [ (sx + x, sy) | x <- [1 .. n] ]
coordsFromDir (sx, sy) ('U', n) = [ (sx, sy - y) | y <- [1 .. n] ]
coordsFromDir (sx, sy) ('D', n) = [ (sx, sy + y) | y <- [1 .. n] ]

