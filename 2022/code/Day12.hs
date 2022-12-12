module Day12 where

import           Data.Char

main :: IO ()
main = do
  m <- lines <$> readFile "inputs/input12.txt"
  putStrLn $ "Part 1 shortest: " ++ show (shortestPath start canWalkUp foundE m)
  putStrLn $ "Part 2 shortest: " ++ show (shortestPath end canWalkDown foundA m)
  return ()

clean :: [String] -> [String]
clean = map (map clean')
 where
  clean' c | c == 'E'  = 'z'
           | c == 'S'  = 'a'
           | otherwise = c

size, end, start :: Pos
size = Pos 136 41
end = Pos 112 20
start = Pos 0 20

shortestPath :: Pos -> CanWalk -> IsFinished -> [String] -> Int
shortestPath start cw isf =
  maximum . map snd . path2 [(start, 0)] [] cw isf . clean


type IsFinished = Pos -> [String] -> Bool

foundE :: IsFinished
foundE p m = p == end && canWalkUp p end m

foundA :: IsFinished
foundA p m = m !! gy p !! gx p == 'a'


type CanWalk = Pos -> Pos -> [String] -> Bool

canWalkUp :: CanWalk
canWalkUp from to m =
  ord (m !! gy from !! gx from) + 1 >= ord (m !! gy to !! gx to)

canWalkDown :: CanWalk
canWalkDown from to m =
  ord (m !! gy from !! gx from) - 1 <= ord (m !! gy to !! gx to)


type Path = [(Pos, Int)]

path2 :: Path -> [Pos] -> CanWalk -> IsFinished -> [String] -> Path
path2 [] _ _ _ _ = error "no path found :("
path2 (c@(p, n) : ps) prior canWalk isFin m
  | isFin p m = [c]
  | otherwise = c : path2 newList newPrior canWalk isFin m
 where
  adj      = adjacent (p, n)
  noDup    = filter (\(p', _) -> p' `notElem` prior) adj
  noSteep  = filter (\(to, _) -> canWalk p to m) noDup
  newPrior = map fst noSteep ++ prior
  newList  = ps ++ noSteep

adjacent :: (Pos, Int) -> [(Pos, Int)]
adjacent (p@(Pos x y), n) = filter outside [r, l, u, d]
 where
  r = (p { gx = x + 1 }, n + 1)
  l = (p { gx = x - 1 }, n + 1)
  u = (p { gy = y - 1 }, n + 1)
  d = (p { gy = y + 1 }, n + 1)
  outside :: (Pos, Int) -> Bool
  outside (a, _) | gx a < 0 || gx a > gx size - 1 = False
                 | gy a < 0 || gy a > gy size - 1 = False
                 | otherwise                      = True

data Pos = Pos { gx :: Int, gy :: Int }
 deriving Eq

instance Show Pos where
  show (Pos x y) = show (x, y)
