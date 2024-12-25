module Day25 where

import           Data.List.Split
import           Data.List

type Key = Bool

main :: IO ()
main = do
  l <- divvy 7 8 . lines <$> readFile "inputs/input25.txt"

  putStrLn $ "Part 1: " ++ show (length $ filter tryLock $ pairs $ parse l)

tryLock :: ((Key, [Int]), (Key, [Int])) -> Bool
tryLock ((p, ns), (q, ms)) = p /= q && all (<= 5) (zipWith (+) ns ms)

pairs :: [a] -> [(a, a)]
pairs l = [ (x, y) | (x : ys) <- tails l, y <- ys ]

parse :: [[String]] -> [(Key, [Int])]
parse = map
  (\ss -> case head (head ss) of
    '.' -> (True, map (`subtract` 6) $ f ss)
    _   -> (False, map (subtract 1) $ f ss)
  )
  where f ss = map (length . head . group) $ transpose ss

