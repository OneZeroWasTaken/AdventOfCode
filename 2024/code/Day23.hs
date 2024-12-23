{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Day23 where

import           Data.Maybe
import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

main :: IO ()
main = do
  l <- parseAll . lines <$> readFile "inputs/input23.txt"

  let m = buildMap l M.empty
      c = map (threeConnected m) (M.keys m)

  putStrLn $ "Part 1: " ++ show ((`div` 3) . sum . map ((`div` 2) . length) $ c)
  putStrLn $ "Part 2: " ++ show (maximumClique m)

maximumClique :: Map String [String] -> [String]
maximumClique m = map snd . last . groupBy (\x y -> fst x == fst y) $ es
 where
  ks = M.keys m
  es = sort $ zip (map (edgesConnected m) ks) ks

edgesConnected :: Map String [String] -> String -> Int
edgesConnected m s =
  length $ filter (\(x, y) -> x `elem` edges y m) $ pairs (edges s m)

edges :: String -> Map String [String] -> [String]
edges s = fromJust . M.lookup s

pairs :: [a] -> [(a, a)]
pairs l = [ (x, y) | (x : ys) <- tails l, y <- ys ]

threeConnected :: Map String [String] -> String -> [String]
threeConnected m s = concatMap
  (filter (elem 't') . (\(f, s3) -> map (\s4 -> [head s, head f, head s4]) s3))
  t1
 where
  f1 = fromJust (M.lookup s m)
  s1 = map (\f -> (f, fromJust (M.lookup f m))) f1
  t1 = map
    (\(f2, s2) -> (f2, filter (\s3 -> s `elem` fromJust (M.lookup s3 m)) s2))
    s1

buildMap :: [(String, String)] -> Map String [String] -> Map String [String]
buildMap []            m = m
buildMap ((k, v) : ss) m = case M.lookup k m of
  Nothing -> buildMap ss (M.insert k [v] m)
  Just _  -> buildMap ss (M.adjust ([v] ++) k m)

parseAll :: [String] -> [(String, String)]
parseAll ss = let first = map parse ss in sort $ first ++ map swap first

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)

parse :: String -> (String, String)
parse s = let [l, r] = splitOn "-" s in (l, r)
