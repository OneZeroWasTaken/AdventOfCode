module Day16 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input16.txt"
  let valves = map parse l
  let vs     = foldl simplify valves valves
  print $ length valves
  print $ length vs
  let start = fromJust $ find ((== "AA") . valve) vs
  print $ search vs (13, 0, start)

  return ()

search :: [Valve] -> (Int, Int, Valve) -> Int
search _  (0, prs, _) = prs
search vs (m, prs, c) = maximum $ prs : openHere ++ map (search vs) mpv
 where
  newM = map ((-) m . dist . snd) curTun
  openHere =
    [ search (openValve c vs) (m - 1, prs + flowRate c * m, c)
    | not $ open c
    , m > 0
    ]
  newPrs      = repeat prs
  newVs       = toGoThrough
  curTun      = tunnels c
  toGoThrough = filter (\v -> valve v `elem` map fst curTun) vs
  mpv         = filter (\(m, _, _) -> m >= 0) $ zip3 newM newPrs newVs

openValve :: Valve -> [Valve] -> [Valve]
openValve v vs = v { open = True } : delete v vs

searchMax :: [Valve] -> Int -> Int -> [Valve] -> Int
searchMax _  0 prs been = prs
searchMax vs m prs been = maximum
  $ map (\v -> searchMax vs newM newPrs (v : been)) toGoThrough
 where
  current     = head been
  newM        = m - 1
  newPrs      = prs + 1
  toGoThrough = filter (\v -> valve v `elem` map fst (tunnels current)) vs
  preasures   = map ((+ prs) . flowRate) toGoThrough
  minutes     = undefined --map (snd) toGoThrough

simplify :: [Valve] -> Valve -> [Valve]
simplify vs v | flowRate v /= 0 || name == "AA" = vs
              | otherwise = replaceAndIncrement name (delete v vs)
  where name = valve v

replaceAndIncrement :: String -> [Valve] -> [Valve]
replaceAndIncrement s = map
  (\v -> v { tunnels = map incrementIfMatch (tunnels v) })
 where
  incrementIfMatch :: (String, Dist) -> (String, Dist)
  incrementIfMatch a@(t, Dist n) | t == s    = (t, Dist $ n + 1)
                                 | otherwise = a

parse :: String -> Valve
parse s = Valve (head a) (read $ a !! 1) False
  $ zip (drop 2 a) (repeat $ Dist 1)
 where
  a = filter (not . null)
    $ splitOneOf "alve has flow rate=; tunnels lead to valves," (tail s)


newtype DistMat = DistMat {distances :: [(Valve, Valve, Int)]} deriving (Show)

dit :: DistMat -> Valve -> Valve -> Int
dit (DistMat []) _ _ = error "Cannot find pair :("
dit (DistMat ((v1, v2, n) : vs)) v3 v4
  | (v1 == v3 || v1 == v4) && (v2 == v3 || v2 == v4) = n
  | otherwise = dit (DistMat vs) v3 v4

pairWiseDist :: [Valve] -> [[Int]] -> [[Int]]
pairWiseDist = undefined

newtype Dist = Dist
    { dist :: Int
    }
 deriving (Show, Eq, Ord)

data Valve = Valve
    { valve :: String
    , flowRate :: Int
    , open :: Bool
    , tunnels :: [(String, Dist)]
    }
  deriving Show

instance Eq Valve where
  v1 == v2 = valve v1 == valve v2

