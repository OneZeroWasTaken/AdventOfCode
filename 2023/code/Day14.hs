module Day13 where

import qualified Data.Set                      as S
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input14.txt"

  let o = posWith ss 'O'
      h = posWith ss '#'
  putStrLn $ "Part 1: " ++ show (load $ north h o S.empty)

  let (t, c) = findCycle h o [o]
      n      = t - c + (1000000000 - t) `mod` 28
  putStrLn $ "Part 2: " ++ show (load $ runN h o n)

type Pos = (Int, Int)

load :: S.Set Pos -> Int
load = S.foldl (\acc (_, y) -> 100 - y + acc) 0

runN :: S.Set Pos -> S.Set Pos -> Int -> S.Set Pos
runN h o n = foldl (\o' c -> c h o') o (replicate n moveCycle)

findCycle :: S.Set Pos -> S.Set Pos -> [S.Set Pos] -> (Int, Int)
findCycle h o acc | o' `elem` acc = (tot, cyc)
                  | otherwise     = findCycle h o' (o' : acc)
 where
  o'         = moveCycle h o
  (tot, cyc) = (length acc, 1 + fromJust (elemIndex o' acc))

moveCycle :: S.Set Pos -> S.Set Pos -> S.Set Pos
moveCycle h o = foldl (\o' m -> m h o' S.empty) o [north, west, south, east]

move
  :: (Pos -> Bool)  -- Ignore function
  -> (Pos -> Pos)   -- Move pos
  -> Next           -- Next pos
  -> S.Set Pos      -- Hashtags
  -> S.Set Pos      -- Unmoved O's
  -> S.Set Pos      -- Acc
  -> S.Set Pos      -- New O's
move ig mv nx h o acc | S.null o  = acc
                      | otherwise = move ig mv nx h o' $ S.insert (move' e) acc
 where
  (e, o') = nx o
  move' p@(x, y) | ig p      = p
                 | mv p `S.member` h || mv p `S.member` acc = (x, y)
                 | otherwise = move' (mv p)

type Next = S.Set Pos -> (Pos, S.Set Pos)

north :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
north = move ((== 0) . snd) (\(x, y) -> (x, y - 1)) S.deleteFindMin

west :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
west = move ((== 0) . fst) (\(x, y) -> (x - 1, y)) S.deleteFindMin

south :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
south = move ((== 99) . snd) (\(x, y) -> (x, y + 1)) S.deleteFindMax

east :: S.Set Pos -> S.Set Pos -> S.Set Pos -> S.Set Pos
east = move ((== 99) . fst) (\(x, y) -> (x + 1, y)) S.deleteFindMax

allPos :: [String] -> S.Set Pos
allPos ss = S.fromList
  [ (x, y) | x <- [0 .. length (head ss) - 1], y <- [0 .. length ss - 1] ]

posWith :: [String] -> Char -> S.Set Pos
posWith ss c = S.filter (\(x, y) -> ss !! y !! x == c) (allPos ss)
