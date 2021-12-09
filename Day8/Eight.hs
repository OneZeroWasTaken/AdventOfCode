module Eight where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  partOne input
  putStrLn ""
  partTwo input
  return ()

partOne :: [String] -> IO ()
partOne l = do
  let count = sum $ map count1478 l
  putStrLn $ "1,4,7,8 count = " ++ show count
  return ()


count1478 :: String -> Int
count1478 = sum . map (is1478 . length) . splitOn " " . dropWhile (/= '|')

is1478 :: Int -> Int
is1478 i | i `elem` [2, 3, 4, 7] = 1
         | otherwise             = 0


partTwo :: [String] -> IO ()
partTwo l = do
  let values = map signalValue l
  putStrLn $ "Part 2 output = " ++ show (sum values)
  return ()

lenSort :: [[a]] -> [[a]]
lenSort = sortBy (\x y -> if length x > length y then GT else LT)

signalValue :: String -> Int
signalValue s = signalValue'
  (lenSort $ map sort $ endBy " " $ takeWhile (/= '|') s)
  (map sort $ tail $ endBy " " $ dropWhile (/= '|') s)
 where
  signalValue' :: [String] -> [String] -> Int
  signalValue' i o = read $ entryValue (mapSignal i 0) o

entryValue :: [(String, Int)] -> [String] -> String
entryValue _ []       = []
entryValue m (o : os) = head (show $ fromJust $ lookup o m) : entryValue m os

mapSignal :: [String] -> Int -> [(String, Int)]
mapSignal _ 10 = []
mapSignal s 0  = (head s, 1) : mapSignal s 1
mapSignal s 1  = (s !! 1, 7) : mapSignal s 2
mapSignal s 2  = (s !! 2, 4) : mapSignal s 3
mapSignal s 3  = get2 s : mapSignal s 4
mapSignal s 4  = get3 s : mapSignal s 5
mapSignal s 5  = get5 s : mapSignal s 6
mapSignal s 6  = get0 s : mapSignal s 7
mapSignal s 7  = get6 s : mapSignal s 8
mapSignal s 8  = get9 s : mapSignal s 9
mapSignal s 9  = (s !! 9, 8) : mapSignal s 10
mapSignal _ _  = []

get0 :: [String] -> (String, Int)
get0 s = (head $ (a069 \\ [fst $ get6 s]) \\ [fst $ get9 s], 0)
  where a069 = [s !! 6, s !! 7, s !! 8]

get6 :: [String] -> (String, Int)
get6 s = (fromJust $ find (\x -> length (x \\ head s) == 5) a069, 6)
  where a069 = [s !! 6, s !! 7, s !! 8]

get9 :: [String] -> (String, Int)
get9 s = (fromJust $ find (\x -> length (x \\ s !! 2) == 2) a069, 9)
  where a069 = [s !! 6, s !! 7, s !! 8]


get2 :: [String] -> (String, Int)
get2 s = (fromJust $ find (\x -> length (s !! 2 \\ x) == 2) a235, 2)
  where a235 = [s !! 3, s !! 4, s !! 5]

get3 :: [String] -> (String, Int)
get3 s = (fromJust $ find (\x -> head s \\ x == "") a235, 3)
  where a235 = [s !! 3, s !! 4, s !! 5]

get5 :: [String] -> (String, Int)
get5 s = (head $ (a235 \\ [fst $ get2 s]) \\ [fst $ get3 s], 5)
  where a235 = [s !! 3, s !! 4, s !! 5]

