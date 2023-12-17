module Day17 where

import           Data.Char
import           Data.Maybe
import           Algorithm.Search               ( dijkstra )

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input17.txt"

  putStrLn $ "Part 1: " ++ show (fst . fromJust . dijk $ ss)
  putStrLn $ "Part 2: " ++ show (fst . fromJust . dijk2 $ ss)

type Pos = (Int, Int)
data Dir = N | E | S | W
 deriving (Eq, Show, Ord, Enum)

data State = State Pos (Dir, Int)
 deriving Show

instance Eq State where
  (State p1 d1) == (State p2 d2) = p1 == p2 && d1 == d2
instance Ord State where
  compare (State p1 d1) (State p2 d2) = compare (p1, d1) (p2, d2)

dijk2 :: [String] -> Maybe (Int, [State])
dijk2 ss = dijkstra neighbors cost (isEnd ss) initialState
 where
  neighbors (State (x, y) ds) =
    filter (goodState2 ss (fst ds))
      $  [ State (x, y - dy) (N, dy) | dy <- [4 .. 10] ]
      ++ [ State (x + dx, y) (E, dx) | dx <- [4 .. 10] ]
      ++ [ State (x, y + dy) (S, dy) | dy <- [4 .. 10] ]
      ++ [ State (x - dx, y) (W, dx) | dx <- [4 .. 10] ]
  cost (State (x1, y1) _) (State (x2, y2) _) = sum $ tail
    [ heat ss (x, y)
    | x <- [min x1 x2 .. max x1 x2]
    , y <- [min y1 y2 .. max y1 y2]
    ]
  initialState = State (0, 0) (W, 0)

goodState2 :: [String] -> Dir -> State -> Bool
goodState2 ss pd (State p ds) = withinBounds ss p && fst ds /= pd && notBack
  where notBack = toEnum ((fromEnum pd + 2) `mod` 4) /= fst ds

dijk :: [String] -> Maybe (Int, [State])
dijk ss = dijkstra neighbors cost (isEnd ss) initialState
 where
  neighbors (State (x, y) ds) = filter
    (goodState ss (fst ds))
    [ State (x    , y - 1) (newDs ds N)
    , State (x + 1, y)     (newDs ds E)
    , State (x    , y + 1) (newDs ds S)
    , State (x - 1, y)     (newDs ds W)
    ]
  newDs (d, n) d' = if d == d' then (d', n + 1) else (d', 1)
  cost _ (State p _) = heat ss p
  initialState = State (0, 0) (E, 0)

goodState :: [String] -> Dir -> State -> Bool
goodState ss pd (State p ds) = withinBounds ss p && snd ds <= 3 && notBack
  where notBack = toEnum ((fromEnum pd + 2) `mod` 4) /= fst ds

isEnd :: [String] -> State -> Bool
isEnd ss s@(State (x, y) _) = x == length (head ss) - 1 && y == length ss - 1

heat :: [String] -> Pos -> Int
heat ss (x, y) = digitToInt $ ss !! y !! x

withinBounds :: [String] -> Pos -> Bool
withinBounds ss (x, y) =
  x >= 0 && x < length (head ss) && y >= 0 && y < length ss
