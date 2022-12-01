module Fifteen where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  let grid  = parseGrid input
  partOne grid
  putStrLn ""
  --partTwo start table
  return ()



partOne :: [[Node]] -> IO ()
partOne grid = do
  let a = aStar grid (0, 0)
  putStrLn $ "Part 1 Path = " ++ show (a)
  return ()

data Node = Node Int Int Bool
 deriving (Eq, Show)

value, weight :: Node -> Int
value (Node _ v _) = v
weight (Node w _ _) = w

visited :: Node -> Bool
visited (Node _ _ v) = v

parseGrid :: [String] -> [[Node]]
parseGrid s = map (map (\c -> Node (digitToInt c) (-1) False)) s

changeEntryTo :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
changeEntryTo a b [] = [(a, b)] -- Note that it adds entry if not present
changeEntryTo entry to ((e, i) : es)
  | e == entry = (e, to) : es
  | otherwise  = (e, i) : changeEntryTo entry to es

type Co = (Int, Int)

--infiniteCoTable :: [((Integer, Integer), Integer)]
--infiniteCoTable = [ [ ((1, 1), 1) | _ <- [0 .. 99] ] | _ <- [0 .. 99] ]

aStar :: [[Node]] -> Co -> [(Co, Co)]
aStar g s = aS g [s] [] startGScore startFScore
 where
  startGScore = changeEntryTo s 0 []
  startFScore = changeEntryTo s (h s) []

aS :: [[Node]] -> [Co] -> [(Co, Co)] -> [(Co, Int)] -> [(Co, Int)] -> [(Co, Co)]
aS g [] cameFrom gScore fScore = error "failiure"
aS g openSet cameFrom gScore fScore
  | (x, y) == goal = newCameFrom
  | otherwise      = aS g newOpenSet newCameFrom newGScore newFScore
 where
  (x, y)      = leastFScore fScore
  current     = g !! y !! x
  better      = filter (betterTentative g current) $ neighborCoords (x, y)
  newCameFrom = updateCameFrom cameFrom better (x, y)
  newGScore   = updateGScore g gScore better (tentative current)
  newFScore   = updateFScore fScore better newGScore
  newOpenSet  = updateOpenSet openSet better

leastFScore :: [(Co, Int)] -> Co
leastFScore fs =
  fst $ minimumBy (\f1 f2 -> if snd f1 < snd f2 then LT else GT) fs


updateOpenSet :: [Co] -> [Co] -> [Co]
updateOpenSet os [] = os
updateOpenSet os (n : ns) | n `elem` os = updateOpenSet os ns
                          | otherwise   = updateOpenSet (n : os) ns

updateFScore :: [(Co, Int)] -> [Co] -> [(Co, Int)] -> [(Co, Int)]
updateFScore fs [] _ = fs
updateFScore fs (n : ns) gs =
  updateFScore (changeEntryTo n (fromJust (lookup n gs) + h n) fs) ns gs

updateGScore :: [[Node]] -> [(Co, Int)] -> [Co] -> (Node -> Int) -> [(Co, Int)]
updateGScore _ gs [] _ = gs
updateGScore g gs (n : ns) t =
  updateGScore g (changeEntryTo n (t $ g !! (snd n) !! (fst n)) gs) ns t

updateCameFrom :: [(Co, Co)] -> [Co] -> Co -> [(Co, Co)]
updateCameFrom cf []       _ = cf
updateCameFrom cf (n : ns) c = updateCameFrom (changeEntryTo n c cf) ns c

updateGrid :: [[Node]] -> [(Int, Int)] -> [[Node]]
updateGrid = undefined

betterTentative :: [[Node]] -> Node -> (Int, Int) -> Bool
betterTentative g current (x, y) = tentative current neighbor < value neighbor
  where neighbor = g !! y !! x

tentative :: Node -> Node -> Int
tentative current neighbor = value current + weight neighbor

goal :: (Int, Int)
goal = (99, 99)

h :: Co -> Int
h (x, y) = fst goal - x + snd goal - y

least :: [Node] -> Int
least n = value
  $ minimumBy (\(Node _ v1 _) (Node _ v2 _) -> if v1 > v2 then GT else LT) n

isVisited :: [[Node]] -> (Int, Int) -> Bool
isVisited g (x, y) = visited $ g !! y !! x

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x, y) = filter
  (\(x', y') -> x' >= 0 && x' < 100 && y' >= 0 && y' < 100)
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

replaceAll :: [(Int, Int)] -> [String] -> [String]
replaceAll [] s = s
replaceAll ((x, y) : cs) s =
  replaceAll cs (replaceAtIndex y (replaceAtIndex x '#' (s !! y)) s)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs

neighbors :: [[Node]] -> (Int, Int) -> [Node]
neighbors g (x, y) = map fromJust $ filter
  isJust
  [coord g (x - 1) y, coord g (x + 1) y, coord g x (y - 1), coord g x (y + 1)]

coord :: [[Node]] -> Int -> Int -> Maybe Node
coord _ (-1) _    = Nothing
coord _ _    (-1) = Nothing
coord _ 100  _    = Nothing
coord _ _    100  = Nothing
coord g x    y    = Just $ g !! y !! x

partTwo :: [[Int]] -> IO ()
partTwo input = do
  putStrLn $ "Part 2 Polymer = " ++ show ()
  return ()

