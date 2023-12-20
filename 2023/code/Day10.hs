module Day10 where

import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input10.txt"
  let s         = findS ss
  let perimiter = pipes ss (connectedTo ss s) s [s]
  putStrLn $ "Part 1: " ++ show (length perimiter `div` 2)
  putStrLn $ "Part 2: " ++ show (picks (shoelace perimiter) (length perimiter))

picks :: Int -> Int -> Int
picks i b = i - b `div` 2 + 1

shoelace :: [Pos] -> Int
shoelace ps = (`div` 2) . abs . sum $ zipWith
  (\(x1, y1) (x2, y2) -> (y1 + y2) * (x1 - x2))
  ps
  (tail ps ++ [head ps])

pipes :: [String] -> Pos -> Pos -> [Pos] -> [Pos]
pipes ss p@(x, y) prev acc | c == 'S'  = acc
                           | otherwise = pipes ss delta p (delta : acc)
 where
  c     = ss !! y !! x
  delta = head $ charToDeltas c p \\ [prev]

charToDeltas :: Char -> Pos -> [Pos]
charToDeltas c (x, y) | c == 'F' = [(x + 1, y), (x, y + 1)]
                      | c == 'L' = [(x + 1, y), (x, y - 1)]
                      | c == 'J' = [(x - 1, y), (x, y - 1)]
                      | c == '7' = [(x - 1, y), (x, y + 1)]
                      | c == '-' = [(x - 1, y), (x + 1, y)]
                      | c == '|' = [(x, y - 1), (x, y + 1)]

type Pos = (Int, Int)

connectedTo :: [String] -> Pos -> Pos
connectedTo ss p@(x, y) = head
  $ filter (\d -> p `elem` charToDeltas (cf d) d) deltas
 where
  cf (x', y') = ss !! y' !! x'
  deltas = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

findS :: [String] -> Pos
findS ss = (f ss, f $ transpose ss) where f = head . mapMaybe (elemIndex 'S')
