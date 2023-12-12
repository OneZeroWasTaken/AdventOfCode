module Day12 where

import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input12.txt"

  let parsed2 = map parseTimes5 l
  putStrLn $ "Part 1: " ++ show (sum $ map (uncurry (val 0) . parse) l)
  putStrLn $ "Part 2: " ++ show ()

val :: Int -> String -> [Int] -> Int
val 0 [] [] = 1
val dssc [] [k] | dssc == k = 1
                | otherwise = 0
val dssc ('?' : s) key = val dssc ('#' : s) key + val dssc ('.' : s) key
val dssc ('#' : s) key = val (dssc + 1) s key
val 0    ('.' : s) key = val 0 s key
val dssc ('.' : s) (k : ks) | dssc == k = val 0 s ks
                            | otherwise = 0
val _ _ _ = 0

parseTimes5 :: String -> (String, [Int])
parseTimes5 s =
  (concat $ tail $ replicate 5 ('?' : sp), concat $ replicate 5 ks)
  where (sp, ks) = parse s

parse :: String -> (String, [Int])
parse s = (sp, map read $ splitOn "," ks) where [sp, ks] = splitOn " " s
