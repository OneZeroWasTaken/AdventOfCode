module Day23 where

import           Data.Char
import           Data.Maybe
import           Algorithm.Search               ( aStar )

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input23test.txt"

  putStrLn $ "Part 1: " ++ show (dijk ss)
  putStrLn $ "Part 2: " ++ show ()

type Pos = (Int, Int)

dijk :: [String] -> Maybe (Int, [Pos])
dijk ss = aStar neighbors cost heuristic isEnd initialState
 where
  neighbors (x, y) = map fst $ filter (validPos ss) (deltas (x, y))
  cost _ _ = -1
  heuristic p | p == (21, 21) = 10000
              | otherwise     = 0
  initialState = (1, 0)
  isEnd        = (== (length ss - 1)) . snd

validPos :: [String] -> (Pos, Char) -> Bool
validPos ss (p, d) = withinBounds ss p && (c == '.' || c == d)
  where c = ss !! snd p !! fst p

withinBounds :: [String] -> Pos -> Bool
withinBounds ss (x, y) =
  x >= 0 && x < length (head ss) && y >= 0 && y < length ss

deltas :: Pos -> [(Pos, Char)]
deltas (x, y) = zip [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)] "^>v<"
