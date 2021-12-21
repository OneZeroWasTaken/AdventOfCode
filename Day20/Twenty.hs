module Twenty where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  --content <- readFile "input.txt"
  content <- readFile "testinput.txt"
  let input   = lines content
  let enhance = head input
  let image   = parseImage $ drop 2 input
  partOne enhance image
  putStrLn ""
  --partTwo
  return ()

parseImage :: [String] -> [(Int, Int)]
parseImage []       = []
parseImage (s : ss) = parseLine s ++ parseImage ss
 where
  parseLine [] = []
  parseLine (c : cs)
    | c == '#'  = (99 - length cs, 99 - length ss) : parseLine cs
    | otherwise = parseLine cs

process :: String -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
process e (100, 99) c acc = acc
process e (100, y ) c acc = process e (0, y + 1) c acc
process e (x  , y ) c acc = process e (x + 1, y) c (algorithm e (x, y) c ++ acc)

algorithm :: String -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
algorithm e (x, y) c | e !! bin == '#' = [(x, y)]
                     | otherwise       = []
  where bin = binListToDec $ coordsToBin (neighbors (x, y)) c


coordsToBin :: [(Int, Int)] -> [(Int, Int)] -> [Int]
coordsToBin [] _ = []
coordsToBin (n : ns) c | n `elem` c = 1 : coordsToBin ns c
                       | otherwise  = 0 : coordsToBin ns c

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (sx, sy) =
  [ (x, y) | y <- reverse [sy - 1 .. sy + 1], x <- [sx - 1 .. sx + 1] ]

partOne :: String -> [(Int, Int)] -> IO ()
partOne e c = do
  let image = process e (0, 0) c []
  printPaper image
  putStrLn $ "Part 1 Image = " ++ show ()
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Distinct Velocities = " ++ show ()
  return ()

binListToDec :: [Int] -> Int
binListToDec l = sum [ (l !! (c - p)) * (2 ^ p) | p <- [0 .. c] ]
  where c = length l - 1

printPaper :: [(Int, Int)] -> IO ()
printPaper paper = do
  let maxX   = maximum (map fst paper)
  let maxY   = maximum (map snd paper)
  let blank  = [ [ '.' | _ <- [0 .. maxX] ] | _ <- [0 .. maxY] ]
  let filled = replaceAll paper blank
  printLines filled 0
  return ()

printLines :: [String] -> Int -> IO ()
printLines p i = if i == length p
  then return ()
  else do
    print $ p !! i
    printLines p (i + 1)
    return ()

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

replaceAll :: [(Int, Int)] -> [String] -> [String]
replaceAll [] s = s
replaceAll ((x, y) : cs) s =
  replaceAll cs (replaceAtIndex y (replaceAtIndex x '#' (s !! y)) s)



