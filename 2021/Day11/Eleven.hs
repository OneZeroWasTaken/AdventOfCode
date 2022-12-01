module Eleven where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  let grid  = map (map digitToInt) input
  partOne grid
  putStrLn ""
  partTwo grid
  return ()

partOne :: [[Int]] -> IO ()
partOne g = do
  let s = stepN g 100
  putStrLn $ "Flashes at step 100 = " ++ show s
  return ()

partTwo :: [[Int]] -> IO ()
partTwo s = do
  let n = recursiveStep s 1
  putStrLn $ "Simultaneous flash at step = " ++ show n
  return ()

size :: Int
size = 9

recursiveStep :: [[Int]] -> Int -> Int
recursiveStep g n | f == (size + 1) * (size + 1) = n
                  | otherwise                    = recursiveStep newG (n + 1)
  where (newG, f) = stepFlash g

stepN :: [[Int]] -> Int -> Int
stepN _ 0 = 0
stepN g n = stepN newG (n - 1) + f where (newG, f) = stepFlash g

stepFlash :: [[Int]] -> ([[Int]], Int)
stepFlash g = (resetFlashed grid, countFlashed grid)
  where grid = recursiveFlash (increaseAllOne g) []

recursiveFlash :: [[Int]] -> [(Int, Int)] -> [[Int]]
recursiveFlash g flashed
  | isJust f && notElem (fromJust f) flashed = recursiveFlash
    (flash g $ fromJust f)
    (fromJust f : flashed)
  | otherwise = g
  where f = findFlash g flashed

flash :: [[Int]] -> (Int, Int) -> [[Int]]
flash g' (x', y') = flash'
  g'
  [ (x, y)
  | x <- [x' - 1 .. x' + 1]
  , x >= 0
  , x < length g'
  , y <- [y' - 1 .. y' + 1]
  , y >= 0
  , y < length g'
  ]
 where
  flash' g []       = g
  flash' g (c : cs) = flash' (increaseOne g c) cs

findFlash :: [[Int]] -> [(Int, Int)] -> Maybe (Int, Int)
findFlash [] _ = Nothing
findFlash (r : rs) flashed | isNothing (findFlashRow r) = findFlash rs flashed
                           | otherwise                  = findFlashRow r
 where
  y = size - length rs
  findFlashRow [] = Nothing
  findFlashRow (f : fs)
    | f > 9 && c `notElem` flashed = Just (size - length fs, y)
    | otherwise                    = findFlashRow fs
    where c = (size - length fs, y)

countFlashed :: [[Int]] -> Int
countFlashed = sum . map (sum . map (\x -> if x > 9 then 1 else 0))

increaseAllOne :: [[Int]] -> [[Int]]
increaseAllOne = map (map (+ 1))

resetFlashed :: [[Int]] -> [[Int]]
resetFlashed = map (map (\x -> if x > 9 then 0 else x))

increaseOne :: [[Int]] -> (Int, Int) -> [[Int]]
increaseOne g (x, y) = rebuild g
 where
  rebuild [] = []
  rebuild (r : rs)
    | length rs == height - y = replaceAtIndex x (value g (x, y) + 1) r
    : rebuild rs
    | otherwise = r : rebuild rs
  height = length g - 1

value :: [[Int]] -> (Int, Int) -> Int
value m (x, y) = m !! y !! x

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs



