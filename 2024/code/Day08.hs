{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Day08 where

import           Data.List

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input08.txt"

  putStrLn $ "Part 1: " ++ show (antinodes l)
  putStrLn $ "Part 2: " ++ show (resonantHarmonics l)

resonantHarmonics :: [String] -> Int
resonantHarmonics ss = length
  $ nub (concatMap (filter (isWithin bnds) . apply [-fst bnds .. fst bnds]) ds')
 where
  bnds = (length $ head ss, length ss)
  ds'  = map (deltas . (`findCoords` ss)) $ unique ss
  apply :: [Int] -> [((Int, Int), (Int, Int))] -> [(Int, Int)]
  apply is ds =
    [ (x + dx * i, y + dy * i) | ((x, y), (dx, dy)) <- ds, i <- is ]

antinodes :: [String] -> Int
antinodes ss =
  length
    . nub
    . concatMap
        ( filter (isWithin bnds)
        . map (uncurry (+-+))
        . deltas
        . (`findCoords` ss)
        )
    $ unique ss
  where bnds = (length $ head ss, length ss)

deltas :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
deltas ps =
  [ ((ax, ay), (ax - bx, ay - by))
  | (ax, ay) <- ps
  , (bx, by) <- ps
  , not (ax - bx == 0 && ay - by == 0)
  ]

unique :: [String] -> String
unique = (\\ ".") . nub . concat

(+-+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) +-+ (x2, y2) = (x1 + x2, y1 + y2)

isWithin :: (Int, Int) -> (Int, Int) -> Bool
isWithin (w, h) (x, y) = x >= 0 && y >= 0 && x < w && y < h

findCoords :: Char -> [String] -> [(Int, Int)]
findCoords ch =
  map fst
    . concatMap
        ( filter ((== ch) . snd)
        . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)
        )
    . zip [0 ..]
