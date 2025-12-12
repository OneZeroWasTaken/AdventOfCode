module Day08 where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tree
import qualified Data.Graph as G

type Vertex = (Int, Int, Int)
type Edge = (Vertex, Vertex)

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input08.txt"

  let vertices = map parse ss
      edges = edgesSorted vertices
      g = graph vertices (take 1000 edges)
      ((x1, _, _), (x2, _, _)) = finalEdge vertices edges
  putStrLn $ "Part 1: " ++ show (product . take 3 . reverse . sort . map (length . flatten) . G.components $ g)
  putStrLn $ "Part 2: " ++ show (x1 * x2)

finalEdge :: [Vertex] -> [Edge] -> Edge
finalEdge vs es = head $ reverse $ head $ filter (\es' -> components vs es' == 1) (inits es)

components :: [Vertex] -> [Edge] -> Int
components vs = length . map flatten . G.components . graph vs

graph :: [Vertex] -> [Edge] -> G.Graph
graph vs es = G.buildG (0, length vs - 1) [(i, j) | (p1, p2) <- es, let i = fromJust (elemIndex p1 vs), let j = fromJust (elemIndex p2 vs)]

edgesSorted :: [Vertex] -> [Edge]
edgesSorted vs = map (\(_, p1, p2) -> (p1, p2)) $ nub $ sort allEdges
  where
    allEdges = [(dist p1 p2, head (sort [p1, p2]), last (sort [p1, p2])) | p1 <- vs, p2 <- vs, p1 /= p2]

dist :: Vertex -> Vertex -> Int
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

parse :: String -> Vertex
parse s = let [a, b, c] = map read (splitOn "," s) in (a, b, c)
