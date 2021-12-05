module Five where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "input.txt"
  let l      = lines content
  let coords = parseCoords l
  let m      = matrix 1000
  let danger = calcDanger m coords
  putStrLn $ "Calculate diagonals: " ++ show partTwo
  putStrLn $ "Danger = " ++ show danger
  return ()

partTwo :: Bool
partTwo = True

calcDanger :: [[Int]] -> [((Int, Int), (Int, Int))] -> Int
calcDanger m [] = overlaps m
calcDanger m ((c1, c2) : cs)
  | isStraight c1 c2 = calcDanger (increaseLine m c1 c2) cs
  | not partTwo      = calcDanger m cs
  | otherwise        = calcDanger (increaseLineDiagonal m c1 c2) cs

-- | Used in part 2
increaseLineDiagonal :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
increaseLineDiagonal m (x1, y1) (x2, y2)
  | x1 == x2 = increaseAll m [ (x1, y) | y <- [min y1 y2 .. max y1 y2] ]
  | y1 == y2 = increaseAll m [ (x, y1) | x <- [min x1 x2 .. max x1 x2] ]
  | otherwise = increaseAll
    m
    [ (leftX + d, startY + d * dy) | d <- [0 .. max x1 x2 - leftX] ]
 where
  leftX  = min x1 x2
  startY = if leftX == x1 then y1 else y2
  endY   = if leftX == x1 then y2 else y1
  dy     = if startY < endY then 1 else -1


isStraight :: (Int, Int) -> (Int, Int) -> Bool
isStraight (x1, y1) (x2, y2) = x1 == x2 || y1 == y2

matrix :: Int -> [[Int]]
matrix s = [ [ 0 | _ <- [0 .. s - 1] ] | _ <- [0 .. s - 1] ]

-- | Used in part 1
increaseLine :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
increaseLine m (x1, y1) (x2, y2)
  | x1 == x2  = increaseAll m [ (x1, y) | y <- [min y1 y2 .. max y1 y2] ]
  | otherwise = increaseAll m [ (x, y1) | x <- [min x1 x2 .. max x1 x2] ]

increaseAll :: [[Int]] -> [(Int, Int)] -> [[Int]]
increaseAll = foldl increase

increase :: [[Int]] -> (Int, Int) -> [[Int]]
increase m (x, y) = rebuild m
 where
  rebuild [] = []
  rebuild (r : rs)
    | length rs == height - y = replaceAtIndex x (value m (x, y) + 1) r
    : rebuild rs
    | otherwise = r : rebuild rs
  height = length m - 1

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

value :: [[Int]] -> (Int, Int) -> Int
value m (x, y) = m !! y !! x

overlaps :: [[Int]] -> Int
overlaps m = sum $ map (sum . map (\d -> if d >= 2 then 1 else 0)) m

parseCoords :: [String] -> [((Int, Int), (Int, Int))]
parseCoords []       = []
parseCoords (l : ls) = ((read x1, read y1), (read x2, read y2))
  : parseCoords ls
  where x1 : y1 : x2 : y2 : _ = filter (not . null) $ splitOneOf ", -> " l

