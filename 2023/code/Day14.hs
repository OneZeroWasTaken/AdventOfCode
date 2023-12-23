module Day13 where

import qualified Data.Set                      as S
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input14.txt"

  let o = posWith ss 'O'
  let h = posWith ss '#'
  putStrLn $ "Part 1: " ++ show (load $ moveUp h o S.empty)
  putStrLn $ "Part 2: " ++ show ()

type Pos = (Int, Int)

load :: S.Set Pos -> Int
load = S.foldl (\acc (_, y) -> 100 - y + acc) 0

moveUp :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
moveUp h o acc | S.null o  = acc
               | otherwise = moveUp h o' $ S.insert (up e) acc
 where
  (e, o') = S.deleteFindMin o
  up p@(x, y) | y == 0    = p
              | (x, y - 1) `S.member` h || (x, y - 1) `S.member` acc = (x, y)
              | otherwise = up (x, y - 1)

allPos :: [String] -> S.Set Pos
allPos ss = S.fromList
  [ (x, y) | x <- [0 .. length (head ss) - 1], y <- [0 .. length ss - 1] ]

posWith :: [String] -> Char -> S.Set Pos
posWith ss c = S.filter (\(x, y) -> ss !! y !! x == c) (allPos ss)
