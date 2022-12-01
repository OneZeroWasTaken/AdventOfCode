module Thirteen where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input          = lines content
  let (paper, folds) = parsePaper input
  partOne paper (head folds)
  putStrLn ""
  partTwo paper folds
  return ()

parsePaper :: [String] -> ([(Int, Int)], [String])
parsePaper s = parse s []
 where
  parse ("" : ss) cs = (cs, map (drop 11) ss)
  parse (c  : ss) cs = parse ss (parseCoord c : cs)
  parse _         _  = ([], [])

parseCoord :: String -> (Int, Int)
parseCoord s =
  (read $ takeWhile (/= ',') s, read $ tail $ dropWhile (/= ',') s)

fold :: Char -> Int -> [(Int, Int)] -> [(Int, Int)]
fold c | c == 'x'  = foldX
       | c == 'y'  = foldY
       | otherwise = error ""

foldX :: Int -> [(Int, Int)] -> [(Int, Int)]
foldX fx = map (\(x, y) -> if x < fx then (x, y) else (fx - x + fx, y))

foldY :: Int -> [(Int, Int)] -> [(Int, Int)]
foldY fy = map (\(x, y) -> if y < fy then (x, y) else (x, fy - y + fy))

partOne :: [(Int, Int)] -> String -> IO ()
partOne paper f = do
  let x  = read $ drop 2 f
  let fp = nub $ foldX x paper
  putStrLn $ "Part 1 Dots = " ++ show (length fp)
  return ()

foldPaper :: [(Int, Int)] -> [String] -> [(Int, Int)]
foldPaper = foldl (\p f -> fold (head f) (read $ drop 2 f) p)

partTwo :: [(Int, Int)] -> [String] -> IO ()
partTwo paper fs = do
  let folded = nub $ foldPaper paper fs
  printPaper folded
  --putStrLn $ "Part 2 = " ++ show ()
  return ()

replaceAll :: [(Int, Int)] -> [String] -> [String]
replaceAll [] s = s
replaceAll ((x, y) : cs) s =
  replaceAll cs (replaceAtIndex y (replaceAtIndex x '#' (s !! y)) s)

printPaper :: [(Int, Int)] -> IO ()
printPaper paper = do
  let maxX  = maximum (map fst paper)
  let maxY  = maximum (map snd paper)
  let blank = [ [ '.' | _ <- [0 .. maxX] ] | _ <- [0 .. maxY] ]
  let code  = replaceAll paper blank
  print $ head code
  print $ code !! 1
  print $ code !! 2
  print $ code !! 3
  print $ code !! 4
  print $ code !! 5
  return ()

-- ARHZPCUH

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs





