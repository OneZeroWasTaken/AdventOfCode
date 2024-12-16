module Day16 where

import Algorithm.Search
import Data.List
import Data.Maybe
import Data.Set qualified as S

data State = State (Int, Int) Dir
  deriving (Show, Ord, Eq)

data Dir = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input16.txt"
  let ps = path ss
  let part1 = fst $ fromJust $ dijk ss ps

  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show (part2 part1 ss ps)

part2 :: Int -> [String] -> [(Int, Int)] -> Int
part2 goal ss ps =
  S.size $
    foldl
      ( \s p ->
          case dijk ss (delete p ps) of
            Just x -> if fst x == goal then S.union s $ S.fromList (map (\(State p' _) -> p') $ snd x) else s
            Nothing -> s
      )
      S.empty
      ps

dijk :: [String] -> [(Int, Int)] -> Maybe (Int, [State])
dijk ss ps = dijkstra (neighborStates ps) cost (isEnd $ end ss) (start ss)

allPaths :: Int -> [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
allPaths goal ss ps acc = case dijkstra (neighborStates ps) cost (isEnd $ end ss) (start ss) of
  Just (c, sts) ->
    if goal == c
      then map (\(State p _) -> p) sts ++ concatMap (\p -> allPaths goal ss (delete p ps) (pathPos ++ acc)) pathPos
      else acc
    where
      pathPos = nub (map (\(State p _) -> p) sts) \\ acc
  Nothing -> acc

next :: (Enum a, Bounded a, Eq a) => a -> a
next x = if x == maxBound then minBound else succ x

previous :: (Enum a, Bounded a, Eq a) => a -> a
previous x = if x == minBound then maxBound else pred x

isEnd :: [State] -> State -> Bool
isEnd es s = s `elem` es

cost :: State -> State -> Int
cost (State _ d1) (State _ d2)
  | d1 == d2 = 1
  | otherwise = 1000

neighborStates :: [(Int, Int)] -> State -> [State]
neighborStates ps (State p d) = [State p (next d), State p (previous d)] ++ [State n d | n `elem` ps]
  where
    n = neighbor p d

neighbor :: (Int, Int) -> Dir -> (Int, Int)
neighbor (x, y) N = (x, y - 1)
neighbor (x, y) E = (x + 1, y)
neighbor (x, y) S = (x, y + 1)
neighbor (x, y) W = (x - 1, y)

path :: [String] -> [(Int, Int)]
path ss = findCoords 'S' ss ++ findCoords 'E' ss ++ findCoords '.' ss

end :: [String] -> [State]
end ss = [State (head $ findCoords 'E' ss) d | d <- [N, E, S, W]]

start :: [String] -> State
start ss = State (head $ findCoords 'S' ss) E

findCoords :: (Eq a) => a -> [[a]] -> [(Int, Int)]
findCoords i = map fst . concatMap (filter ((== i) . snd) . (\(y, l) -> zipWith (\x c -> ((x, y), c)) [0 ..] l)) . zip [0 ..]