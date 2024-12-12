{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day12 where

import Data.List

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input12.txt"

  let rs = regions ss [(x, y) | y <- [0 .. length ss - 1], x <- [0 .. length (head ss) - 1]]

  putStrLn $ "Part 1: " ++ show (price perimeter rs)
  putStrLn $ "Part 2: " ++ show (price (\r -> perimeter r - sum (map (`surplusSides` r) (neighbors (0, 0)))) rs)

price :: ([(Int, Int)] -> Int) -> [[(Int, Int)]] -> Int
price f rs = sum $ zipWith (*) (map f rs) (map length rs)

regions :: [String] -> [(Int, Int)] -> [[(Int, Int)]]
regions _ [] = []
regions ss (p' : ps') = let r = region (ss @. p') [p'] [] in r : regions ss (ps' \\ r)
  where
    region :: Char -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    region _ [] acc = acc
    region c (p : ps) acc
      | ss @. p == c =
          let n = filter (`notElem` (ps ++ acc)) . filter (inBounds ss) . neighbors $ p
           in region c (n ++ ps) (p : acc)
      | otherwise = region c ps acc

surplusSides :: (Int, Int) -> [(Int, Int)] -> Int
surplusSides (dx, dy) ps = sum adjacent `div` 2
  where
    edge = filter (`notElem` ps) . map (<+> (dx, dy)) $ ps
    adjacent = map (length . filter (`elem` edge) . neighbors) edge

perimeter :: [(Int, Int)] -> Int
perimeter ps = length $ concatMap (filter (`notElem` ps) . neighbors) ps

(@.) :: [[a]] -> (Int, Int) -> a
m @. (x, y) = m !! y !! x

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

inBounds :: [String] -> (Int, Int) -> Bool
inBounds ss (x, y) = x >= 0 && y >= 0 && x < length ss && y < length (head ss)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
