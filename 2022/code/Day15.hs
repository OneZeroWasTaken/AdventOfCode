module Day15 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input15.txt"
  let ss   = map (parseSensor . parsePos) l
  let minX = minimum $ map (gx . pos) ss
  let maxX = maximum $ map (gx . pos) ss
  let maxMan        = maximum $ map man ss
      pos           = [ Pos x row | x <- [minX - maxMan .. maxX + maxMan] ]
      bsOnSameRow   = beaconsAtY row (nub $ map beacon ss)
      cannotContain = cannotHaveBeaconCount ss pos - bsOnSameRow

  let beaconPos = head $ concatMap (edges ss) ss

  putStrLn $ "Part 1:      Can contain=" ++ show cannotContain
  putStrLn $ "Part 2:  Beacon position=" ++ show beaconPos
  putStrLn $ "        Tuning frequency=" ++ show (tuningFreq beaconPos)

  return ()

edges :: [Sensor] -> Sensor -> [Pos]
edges ss sensor = a ++ b ++ c ++ d
 where
  p  = pos sensor
  m  = man sensor
  os = ss \\ [sensor]
  a  = filter (\p -> inside p && canHaveBeacon os p)
              [ Pos (gx p + m - i + 1) (gy p + i) | i <- [0 .. m] ]
  b = filter (\p -> inside p && canHaveBeacon os p)
             [ Pos (gx p - i) (gy p + m - i + 1) | i <- [0 .. m] ]
  c = filter (\p -> inside p && canHaveBeacon os p)
             [ Pos (gx p - m + i - 1) (gy p - i) | i <- [0 .. m] ]
  d = filter (\p -> inside p && canHaveBeacon os p)
             [ Pos (gx p + i) (gy p - m + i - 1) | i <- [0 .. m] ]

inside :: Pos -> Bool
inside (Pos x y) | x < 0 || y < 0       = False
                 | x > edge || y > edge = False
                 | otherwise            = True

beaconsAtY :: Int -> [Pos] -> Int
beaconsAtY y = length . filter ((== y) . gy)

parsePos :: String -> (Pos, Pos)
parsePos s = (Pos a b, Pos c d)
 where
  [a, b, c, d] =
    map read
      . filter (not . null)
      . splitOneOf "Sensor at x=, y=: closest beacon is at x=, y="
      $ s

parseSensor :: (Pos, Pos) -> Sensor
parseSensor (s, b) = Sensor s b (manhattan s b)

manhattan :: Pos -> Pos -> Int
manhattan a b = abs (gx a - gx b) + abs (gy a - gy b)

canHaveBeacon :: [Sensor] -> Pos -> Bool
canHaveBeacon [] _ = True
canHaveBeacon (s : ss) p | manhattan p (pos s) <= man s = False
                         | otherwise                    = canHaveBeacon ss p

cannotHaveBeaconCount :: [Sensor] -> [Pos] -> Int
cannotHaveBeaconCount ss = length . filter (not . canHaveBeacon ss)

tuningFreq :: Pos -> Integer
tuningFreq (Pos x y) = toInteger x * toInteger edge + toInteger y

row :: Int
row = 2000000

edge :: Int
edge = 4000000

data Sensor = Sensor
  { pos :: Pos
  , beacon :: Pos
  , man :: Int
  } deriving (Show, Eq)

data Pos = Pos { gx :: Int, gy :: Int }
 deriving Eq

instance Show Pos where
  show (Pos x y) = show (x, y)
