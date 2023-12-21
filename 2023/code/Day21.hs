module Day21 where

import qualified Data.Set                      as S
import qualified Data.Set.Extra                as E
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input21.txt"

  let part1 = foldr walk (S.singleton $ findS ss) (replicate 64 ss)
  putStrLn $ "Part 1: " ++ show (length part1)
  putStrLn $ "Part 2: " ++ show (calculate ss steps)

steps :: Int
steps = 26501365

diamond :: Int -> (Int, Int)
diamond n = ((n + 1) ^ 2, n ^ 2)

type Parity = Bool

calculate :: [String] -> Int -> Int
calculate ss steps =
  oddParity * oddN + evenParity * evenN - (n + 1) * oddCorners + n * evenCorners
 where
  n                       = steps `div` length ss
  (oddParity, evenParity) = diamond n
  oddN                    = length $ positions ss True
  evenN                   = length $ positions ss False
  oddCorners              = length $ S.filter corners $ positions ss True
  evenCorners             = length $ S.filter corners $ positions ss False
  corners p = manhattan p (findS ss) > (length ss `div` 2)

positions :: [String] -> Parity -> S.Set Pos
positions ss parity = S.filter (filterPos ss parity) (allPos ss)

filterPos :: [String] -> Parity -> Pos -> Bool
filterPos ss parity p = isGarden ss p && correctParity p && isReachable
 where
  center = findS ss
  mh p = manhattan p center
  correctParity p = if parity then odd $ mh p else even $ mh p
  isReachable = length (foldr walk (S.singleton p) (replicate 8 ss)) > 4

allPos :: [String] -> S.Set Pos
allPos ss = S.fromList
  [ (x, y) | x <- [0 .. length (head ss) - 1], y <- [0 .. length ss - 1] ]

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

type Pos = (Int, Int)

walk :: [String] -> S.Set Pos -> S.Set Pos
walk ss s = E.flatten
  $ S.map (\p -> S.fromList (filter (isValid ss) $ map (p <+>) deltas)) s

isValid :: [String] -> Pos -> Bool
isValid ss (x, y) = isInside && isGarden ss (x, y)
  where isInside = x >= 0 && x < length (head ss) && y >= 0 && y < length ss

isGarden :: [String] -> Pos -> Bool
isGarden ss (x, y) = ss !! y !! x /= '#'

(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

findS :: [String] -> Pos
findS ss = (f ss, f $ transpose ss) where f = head . mapMaybe (elemIndex 'S')

deltas :: [Pos]
deltas = [(1, 0), (0, 1), (-1, 0), (0, -1)]
