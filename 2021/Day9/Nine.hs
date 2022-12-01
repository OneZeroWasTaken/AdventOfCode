module Nine where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  partOne input
  putStrLn ""
  partTwo input
  return ()

partTwo :: [String] -> IO ()
partTwo s = do
  let b = basinFinder s [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ] []
  let sorted = sortOn Down b
  let last3  = take 3 sorted
  print last3
  putStrLn $ "Part two answer = " ++ show (product last3)
  return ()

basinFinder :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
basinFinder _ [] _ = []
basinFinder s ((x, y) : coords) been
  | coord s x y == Just 9 = basinFinder s coords been
  | (x, y) `elem` been    = basinFinder s coords been
  | otherwise             = basinSize : basinFinder s coords (been' ++ been)
 where
  (basinSize, been') = recursive [(x, y)] 0 []
  recursive [] size b = (size, b)
  recursive (c : cs) size b
    | c `elem` b = recursive cs size b
    | otherwise  = recursive (neighbors3 s c ++ cs) (size + 1) (c : b)


neighbors3 :: [String] -> (Int, Int) -> [(Int, Int)]
neighbors3 s (x, y) = map
  snd
  (filter (\(n, _) -> n /= Just 9) $ filter (isJust . fst) $ neighbors2 s (x, y)
  )

neighbors2 :: [String] -> (Int, Int) -> [(Maybe Int, (Int, Int))]
neighbors2 s (x, y) =
  [ (coord s (x - 1) y, (x - 1, y))
  , (coord s (x + 1) y, (x + 1, y))
  , (coord s x (y - 1), (x, y - 1))
  , (coord s x (y + 1), (x, y + 1))
  ]

partOne :: [String] -> IO ()
partOne l = do
  let low = [ lowPoint l (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]
  let vals = filterLowPoints low
  --putStrLn $ "Low points = " ++ show vals
  putStrLn $ "Risk level = " ++ show (sum $ map (+ 1) vals)
  return ()

filterLowPoints :: [Maybe Int] -> [Int]
filterLowPoints []             = []
filterLowPoints (Nothing : es) = filterLowPoints es
filterLowPoints (Just n  : es) = n : filterLowPoints es

lowPoint :: [String] -> (Int, Int) -> Maybe Int
lowPoint s (x, y) | minimum (filter isJust $ neighbors s (x, y)) > n = n
                  | otherwise = Nothing
  where n = coord s x y

neighbors :: [String] -> (Int, Int) -> [Maybe Int]
neighbors s (x, y) =
  [coord s (x - 1) y, coord s (x + 1) y, coord s x (y - 1), coord s x (y + 1)]

coord :: [String] -> Int -> Int -> Maybe Int
coord _ (-1) _    = Nothing
coord _ _    (-1) = Nothing
coord _ 100  _    = Nothing
coord _ _    100  = Nothing
coord s x    y    = Just $ digitToInt (s !! y !! x)

