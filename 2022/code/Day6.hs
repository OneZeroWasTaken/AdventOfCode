module Day6 where

import           Data.List

main :: IO ()
main = do
  l <- readFile "inputs/input6.txt"
  putStrLn $ "Part 1 (4 chars): " ++ show (firstMarker 4 l)
  putStrLn $ "Part 2 (14 chars): " ++ show (firstMarker 14 l)
  return ()

firstMarker :: Int -> String -> Int
firstMarker n = head . findMarkers . splitN n

findMarkers :: [String] -> [Int]
findMarkers ms =
  [ chars | (ism, chars) <- zip (map isMarker ms) [length (head ms) ..], ism ]

isMarker :: String -> Bool
isMarker s = nub s == s

splitN :: Int -> String -> [String]
splitN n ss = [ take n $ drop i ss | i <- [0 .. length ss - n] ]

