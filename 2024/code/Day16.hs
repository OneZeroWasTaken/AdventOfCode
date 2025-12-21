{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day16 where

import           Algorithm.Search
import           Data.List
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

data State = State (Int, Int) Dir
  deriving (Show, Ord, Eq)

data Dir = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input16.txt"
  let ps    = path ss
  let part1 = fst $ fromJust $ dijk ss ps

  putStrLn $ "Part 1: " ++ show part1
  --let my = myDijk ps (M.fromList [(start ss, 0)]) [start ss]
  --putStrLn $ "Part 2: " ++ show (map (`M.lookup` my) $ end ss)

extPos :: (Int, [State]) -> [(Int, Int)]
extPos (_, sts) = map (\(State p _) -> p) sts

myDijk :: [(Int, Int)] -> Map State Int -> [State] -> Map State Int
myDijk _  m []       = m
myDijk ps m (c : rs) = myDijk ps nm (rs ++ ncs)
 where
  ns  = neighborStates ps c
  ncs = filter (\e -> isNothing $ M.lookup e m) ns
  cc  = fromJust $ M.lookup c m
  nm  = foldr
    (\s m' -> case M.lookup s m' of
      Nothing -> M.insert s (cc + cost c s) m'
      Just ct ->
        if cc + cost c s < ct then M.insert s (cc + cost c s) m' else m'
    )
    m
    ns

dijk :: [String] -> [(Int, Int)] -> Maybe (Int, [State])
dijk ss ps = dijkstra (neighborStates ps) cost (isEnd $ end ss) (start ss)

next :: (Enum a, Bounded a, Eq a) => a -> a
next x = if x == maxBound then minBound else succ x

previous :: (Enum a, Bounded a, Eq a) => a -> a
previous x = if x == minBound then maxBound else pred x

isEnd :: [State] -> State -> Bool
isEnd es s = s `elem` es

cost :: State -> State -> Int
cost (State _ d1) (State _ d2) | d1 == d2  = 1
                               | otherwise = 1000

neighborStates :: [(Int, Int)] -> State -> [State]
neighborStates ps (State p d) =
  [State p (next d), State p (previous d)] ++ [ State n d | n `elem` ps ]
  where n = neighbor p d

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

neighbor :: (Int, Int) -> Dir -> (Int, Int)
neighbor (x, y) N = (x, y - 1)
neighbor (x, y) E = (x + 1, y)
neighbor (x, y) S = (x, y + 1)
neighbor (x, y) W = (x - 1, y)

path :: [String] -> [(Int, Int)]
path ss = findCoords 'S' ss ++ findCoords 'E' ss ++ findCoords '.' ss

end :: [String] -> [State]
end ss = [ State (head $ findCoords 'E' ss) d | d <- [N, E, S, W] ]

start :: [String] -> State
start ss = State (head $ findCoords 'S' ss) E

findCoords :: (Eq a) => a -> [[a]] -> [(Int, Int)]
findCoords i =
  map fst
    . concatMap
        ( filter ((== i) . snd)
        . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)
        )
    . zip [0 ..]
