module Day3 where

import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "inputs/input3.txt"
  let l    = lines content
  let prio = sum $ map (priority . head . duplicates . half) l
  putStrLn $ "Part 1 priority: " ++ show prio
  putStrLn $ "Part 2 priority: " ++ show (threeSplitPrio l)
  return ()

threeSplitPrio :: [String] -> Int
threeSplitPrio (a : b : c : ss) =
  (priority . head . duplicates3) (a, b, c) + threeSplitPrio ss
threeSplitPrio _ = 0

duplicates3 :: (String, String, String) -> [Char]
duplicates3 (m, n, o) =
  concat $ map (\c -> if c `elem` n && c `elem` o then [c] else []) m

half :: [a] -> ([a], [a])
half s = splitAt (length s `div` 2) s

duplicates :: (String, String) -> [Char]
duplicates (f, s) = concat $ map (\c -> if c `elem` s then [c] else []) f

priority :: Char -> Int
priority c = fromJust $ lookup c $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]
