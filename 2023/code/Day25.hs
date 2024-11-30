{-# LANGUAGE InstanceSigs #-}
module Day25 where

import qualified Data.Set                      as S
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input25.txt"

  let g = foldl1 merge $ map parse l
  let a = contract g (Edge "xqj" "xtr")
  putStrLn $ "Part 1: " ++ show (g)

contract :: Graph -> Edge -> Graph
contract (Graph vs es) (Edge e1 e2) = undefined
 where
  v = e1 ++ e2
  a = S.map
    (\Edge e f ->
      if e1 `elem` [e, f] || e2 `elem` [e, f] then undefined else undefined
    )
    es


merge :: Graph -> Graph -> Graph
merge (Graph v1 e1) (Graph v2 e2) = Graph (v1 `S.union` v2) (e1 `S.union` e2)

data Graph = Graph (S.Set String) (S.Set Edge)
 deriving (Eq, Show)

data Edge = Edge String String
 deriving Show

instance Eq Edge where
  (==) :: Edge -> Edge -> Bool
  (Edge s1 s2) == (Edge s3 s4) = (s1, s2) == (s3, s4) || (s2, s1) == (s3, s4)
instance Ord Edge where
  compare :: Edge -> Edge -> Ordering
  compare (Edge s1 s2) (Edge s3 s4) =
    compare (min s1 s2, max s1 s2) (min s3 s4, max s3 s4)

parse :: String -> Graph
parse l = Graph (S.fromList v) (S.fromList $ map (uncurry Edge) (pairs v))
  where v = filter (not . null) . splitOneOf ": " $ l

pairs :: [a] -> [(a, a)]
pairs l = [ (x, y) | (x : ys) <- tails l, y <- ys ]
