module Day12 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input12.txt"

  let parsed2 = take 4 $ map parseTimes5 l
  --let parsed2 = [parseTimes5 "???.### 1,1,3"]
  --putStrLn $ "Part 1: " ++ show (sum $ map (uncurry (val 0) . parse) l)
  putStrLn $ "Part 2: " ++ show (sum $ map (uncurry val2) parsed2)

val2 :: String -> [Int] -> Int
val2 [] [] = 1
val2 [] _  = 0
val2 s [] | '#' `notElem` s = 1
          | otherwise       = 0
val2 (s : ss) (n : ns)
  | s == '#' && matchesPattern (s : ss) pt = val2 newS ns
  | s == '#'                               = 0
  | matchesPattern (s : ss) pt             = val2 ss (n : ns) + val2 newS ns
  | otherwise                              = val2 ss (n : ns)
 where
  pt   = pattern' n
  newS = drop n ss

matchesPattern :: String -> String -> Bool
matchesPattern [] "." = True
matchesPattern _  []  = True
matchesPattern [] _   = False
matchesPattern (s : ss) (p : pp) | s == p    = matchesPattern ss pp
                                 | s == '?'  = matchesPattern ss pp
                                 | otherwise = False

pattern' :: Int -> String
pattern' n = replicate n '#' ++ "."

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
  (tail $ concat $ replicate 5 ('?' : sp), concat $ replicate 5 ks)
  where (sp, ks) = parse s

parse :: String -> (String, [Int])
parse s = (sp, map read $ splitOn "," ks) where [sp, ks] = splitOn " " s
