module Day15 where

import           Data.List.Split
import           Data.List
import           Data.Maybe

data Dir = N | E | S | W
 deriving (Show, Eq)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input15.txt"

  let ((ws, os, s), ds) = parse l
  putStrLn $ "Part 1: " ++ show (sum $ map score $ run ws s ds os)
  putStrLn $ "Part 2: " ++ show ()
    -- (sum $ map score2 $ run2 ws
                             -- (fst s, snd s, False)
                             -- ds
                             -- (map (\(x, y) -> (x, y, False)) os)
    -- )
score2 = undefined

type Half = Bool

-- run2
  -- :: [(Int, Int)]
  -- -> (Int, Int, Half)
  -- -> [Dir]
  -- -> [(Int, Int, Half)]
  -- -> [(Int, Int, Half)]
-- run2 _ _ [] os = os
-- run2 ws p (d : ds) os
  -- | coarseP `elem` ws = run2 ws p ds os
  -- | newP `boxCollide` os = case nextHoles newP of
    -- Nothing -> run2 ws p ds os
    -- Just c  -> run2 ws newP ds (c : delete newP os)
  -- | otherwise = run2 ws newP ds os
 -- where
  -- newP    = moveHalf p d
  -- coarseP = (\(x, y, _) -> (x, y)) newP
  -- nextHoles :: (Int, Int, Half) -> Maybe [(Int, Int, Half)]
  -- nextHoles (x, y, h) | d `elem` [W, E] && h     = Nothing
                      -- | d `elem` [W, E] && not h = Nothing
                      -- | c `elem` ws              = Nothing
                      -- | c `notElem` os           = Just c
                      -- | otherwise                = nextHoles (move c d)

boxCollide :: (Int, Int, Half) -> [(Int, Int, Half)] -> Bool
boxCollide p@(x, y, True ) os = p `elem` os || (x, y, False) `elem` os
boxCollide p@(x, y, False) os = p `elem` os || (x - 1, y, True) `elem` os

moveHalf :: (Int, Int, Half) -> Dir -> (Int, Int, Half)
moveHalf (x, y, h    ) N = (x, y - 1, h)
moveHalf (x, y, h    ) S = (x, y + 1, h)
moveHalf (x, y, True ) W = (x, y, False)
moveHalf (x, y, False) W = (x - 1, y, True)
moveHalf (x, y, True ) E = (x + 1, y, False)
moveHalf (x, y, False) E = (x, y, True)

score :: (Int, Int) -> Int
score (x, y) = x + 100 * y

run :: [(Int, Int)] -> (Int, Int) -> [Dir] -> [(Int, Int)] -> [(Int, Int)]
run _ _ [] os = os
run ws p (d : ds) os
  | newP `elem` ws = run ws p ds os
  | newP `elem` os = case nextHole newP of
    Nothing -> run ws p ds os
    Just c  -> run ws newP ds (c : delete newP os)
  | otherwise = run ws newP ds os
 where
  newP = move p d
  nextHole :: (Int, Int) -> Maybe (Int, Int)
  nextHole c | c `elem` ws    = Nothing
             | c `notElem` os = Just c
             | otherwise      = nextHole (move c d)

deltas :: [(Int, Int)]
deltas = [(0, -1), (1, 0), (0, 1), (-1, 0)]

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) N = (x, y - 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

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
