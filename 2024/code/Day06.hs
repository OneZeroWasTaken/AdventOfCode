{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 where

import Data.List

-- 564 low
-- 2151 high
main :: IO ()
main = do
  -- l <- lines <$> readFile "inputs/input06.txt"
  let l = lines "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

  let bounds = (length $ head l, length l)

  putStrLn $ "Part 1: " ++ show (length $ nub $ traverse' bounds (obstacles l) [start l] N)
  -- putStrLn $ "Part 2: " ++ show (loops bounds (obstacles l) [start l] N)
  let a = traverse' bounds (obstacles l) [start l] N
  let poss = tail $ traverse' bounds (obstacles l) [start l] N
  let b = map (\pos -> willLoop bounds (pos : obstacles l) [(N, fst $ start l, snd $ start l)] N) poss
  putStrLn $ "Part 2: " ++ show (length $ filter id b)

oo = obstacles $ lines "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

testObstacles :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Int
testObstacles s bounds [] _ = 0
testObstacles s bounds (o : os) obs
  | willLoop bounds (o : obs) [(N, fst s, snd s)] N = 1 + testObstacles s bounds os obs
  | otherwise = testObstacles s bounds os (o : obs)

willLoop :: (Int, Int) -> [(Int, Int)] -> [(Dir, Int, Int)] -> Dir -> Bool
willLoop _ _ [] _ = False
willLoop bounds obs (p@(_, x, y) : ps) dir
  | isOutside bounds (x, y) = False
  | p `elem` ps = True
  | (x, y) `elem` obs = willLoop bounds obs ps (turn dir)
  | otherwise = willLoop bounds obs ((dir, nx, ny) : p : ps) dir
  where
    (nx, ny) = go (x, y) dir

traverse' :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Dir -> [(Int, Int)]
traverse' bounds obs (p : ps) dir
  | isOutside bounds p = ps
  | p `elem` obs = traverse' bounds obs ps (turn dir)
  | otherwise = traverse' bounds obs (newP : p : ps) dir
  where
    newP = go p dir

-- loops :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Dir -> Int
-- loops bounds obs (p : ps) dir
-- \| isOutside bounds p = 0
-- \| p `elem` obs = loops bounds obs ps (turn dir)
-- \| willLoop bounds (newP : obs) [(turn dir, fst p, snd p)] dir = 1 + loops bounds obs (newP : p : ps) dir
-- \| otherwise = loops bounds obs (newP : p : ps) dir
-- where
-- newP = go p dir

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

start :: [String] -> (Int, Int)
start = head . findCoords '^'

obstacles :: [String] -> [(Int, Int)]
obstacles = findCoords '#'

findCoords :: Char -> [String] -> [(Int, Int)]
findCoords ch = map fst . concatMap (filter ((== ch) . snd) . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)) . zip [0 ..]
