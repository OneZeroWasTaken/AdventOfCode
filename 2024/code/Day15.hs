module Day15 where

import Data.List
import Data.Maybe

data Dir = N | E | S | W
  deriving (Show, Eq)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input15.txt"

  let ((ws, os, s), ds) = parse l
  putStrLn $ "Part 1: " ++ show (sum $ map score $ run ws s ds os)

score :: (Int, Int) -> Int
score (x, y) = x + 100 * y

run :: [(Int, Int)] -> (Int, Int) -> [Dir] -> [(Int, Int)] -> [(Int, Int)]
run _ _ [] os = os
run ws p (d : ds) os
  | newP `elem` ws = run ws p ds os
  | newP `elem` os = case nextHole newP of
      Nothing -> run ws p ds os
      Just c -> run ws newP ds (c : delete newP os)
  | otherwise = run ws newP ds os
  where
    newP = move p d
    nextHole :: (Int, Int) -> Maybe (Int, Int)
    nextHole c
      | c `elem` ws = Nothing
      | c `notElem` os = Just c
      | otherwise = nextHole (move c d)

deltas :: [(Int, Int)]
deltas = [(0, -1), (1, 0), (0, 1), (-1, 0)]

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) N = (x, y - 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)

parse :: [String] -> (([(Int, Int)], [(Int, Int)], (Int, Int)), [Dir])
parse ss = (parseGrid ss, parseDir $ concat $ drop 50 ss)

parseDir :: String -> [Dir]
parseDir =
  map (\d -> fromJust (lookup d [('^', N), ('>', E), ('v', S), ('<', W)]))

parseGrid :: [String] -> ([(Int, Int)], [(Int, Int)], (Int, Int))
parseGrid ss = (findCoords '#' ss, findCoords 'O' ss, head $ findCoords '@' ss)

findCoords :: (Eq a) => a -> [[a]] -> [(Int, Int)]
findCoords i =
  map fst
    . concatMap
      ( filter ((== i) . snd)
          . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)
      )
    . zip [0 ..]
