{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 where

import           Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input06.txt"

  let bounds = (length $ head l, length l)
      obs    = obstacles l
      start' = start l

  let distinctPos = nub $ traverse' bounds obs start' N
  putStrLn $ "Part 1: " ++ (show . length) distinctPos

  let (x, y) = head start'
  let part2 = length $ filter
        (\pos -> willLoop bounds (pos : obs) [(N, x, y)] N)
        (distinctPos \\ start')

  putStrLn $ "Part 2: " ++ show part2

willLoop :: (Int, Int) -> [(Int, Int)] -> [(Dir, Int, Int)] -> Dir -> Bool
willLoop _ _ [] _ = False
willLoop bounds obs (p@(_, x, y) : ps) dir
  | isOutside bounds (x, y) = False
  | p `elem` ps             = True
  | (x, y) `elem` obs       = willLoop bounds obs ps (turn dir)
  | otherwise               = willLoop bounds obs ((dir, nx, ny) : p : ps) dir
  where (nx, ny) = go (x, y) dir

traverse' :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Dir -> [(Int, Int)]
traverse' bounds obs (p : ps) dir
  | isOutside bounds p = ps
  | p `elem` obs       = traverse' bounds obs ps (turn dir)
  | otherwise          = traverse' bounds obs (newP : p : ps) dir
  where newP = go p dir

go :: (Int, Int) -> Dir -> (Int, Int)
go (x, y) N = (x, y - 1)
go (x, y) E = (x + 1, y)
go (x, y) S = (x, y + 1)
go (x, y) W = (x - 1, y)

isOutside :: (Int, Int) -> (Int, Int) -> Bool
isOutside (w, h) (x, y) = x < 0 || y < 0 || x >= w || y >= h

data Dir = N | E | S | W deriving (Show, Eq)

turn :: Dir -> Dir
turn N = E
turn E = S
turn S = W
turn W = N

start :: [String] -> [(Int, Int)]
start = findCoords '^'

obstacles :: [String] -> [(Int, Int)]
obstacles = findCoords '#'

findCoords :: Char -> [String] -> [(Int, Int)]
findCoords ch =
  map fst
    . concatMap
        ( filter ((== ch) . snd)
        . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)
        )
    . zip [0 ..]
