module Day11 where

import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input11.txt"
  let sts = map parse l

  putStrLn $ "Part 1: " ++ show (countPaths "you" "out" (next sts))
  putStrLn $ "Part 2: " ++ show (countPathsAdvanced (next sts) ["svr", "fft", "dac", "out"])

countPathsAdvanced :: (String -> [String]) -> [String] -> Int
countPathsAdvanced next ss = product $ map (\(s, g) -> travel next [(s, 1)] g) (zip ss $ tail ss)

travel :: (String -> [String]) -> [(String, Int)] -> String -> Int
travel _ [] _ = 0
travel next here goal = sum (map snd gs) + travel next there goal
  where
    (gs, there) = partition ((== goal) . fst) (merge $ sort $ concatMap travelOne here)

    travelOne :: (String, Int) -> [(String, Int)]
    travelOne (s, n) = next s >>= \ns -> return (ns, n)
    merge :: [(String, Int)] -> [(String, Int)]
    merge (a:b:rs) | fst a == fst b = merge ((fst a, snd a + snd b) : rs)
                       | otherwise = a : merge (b:rs)
    merge e = e

countPaths :: String -> String -> (String -> [String]) -> Int
countPaths start goal next
  | start == goal = 1
  | otherwise = sum [countPaths n goal next | n <- next start]

next :: [(String, [String])] -> String -> [String]
next sts s
  | s == "out" = []
  | otherwise = fromJust $ lookup s sts

parse :: String -> (String, [String])
parse s = let (a : as) = filter (not . null) $ splitOneOf ": " s in (a, as)
