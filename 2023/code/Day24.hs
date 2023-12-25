module Day24 where

import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input24.txt"

  let hails = map (toLine . parseXY) l
  putStrLn $ "Part 1: " ++ show (totalIntersections hails)
  putStrLn $ "Part 2: " ++ show ()

totalIntersections :: [Line] -> Int
totalIntersections = length . mapMaybe (uncurry lineIntersect) . pairs

toLine :: Hail -> Line
toLine h = Line h k m
 where
  k = vy h / vx h
  m = py h - k * px h

lineIntersect :: Line -> Line -> Maybe (Double, Double)
lineIntersect (Line h1 a c) (Line h2 b d)
  | a == b    = Nothing
  | withinBounds (x, y) && isAfter h1 && isAfter h2 = Just (x, y)
  | otherwise = Nothing
 where
  x = (d - c) / (a - b)
  y = a * x + c
  isAfter h = signum (x - px h) == signum (vx h)

data Line = Line Hail Double Double
 deriving (Show, Eq)

data Hail = Hail Int Int Int Int Int Int
 deriving (Show, Eq)

px, py, pz, vx, vy, vz :: Hail -> Double
px (Hail a _ _ _ _ _) = fromIntegral a
py (Hail _ b _ _ _ _) = fromIntegral b
pz (Hail _ _ c _ _ _) = fromIntegral c
vx (Hail _ _ _ d _ _) = fromIntegral d
vy (Hail _ _ _ _ e _) = fromIntegral e
vz (Hail _ _ _ _ _ f) = fromIntegral f

withinBounds :: (Double, Double) -> Bool
withinBounds (x, y) =
  x >= fst bounds && x <= snd bounds && y >= fst bounds && y <= snd bounds

bounds :: (Double, Double)
bounds = (200000000000000, 400000000000000)

pairs :: [a] -> [(a, a)]
pairs l = [ (x, y) | (x : ys) <- tails l, y <- ys ]

parseXY :: String -> Hail
parseXY s = Hail x y z vx vy vz
 where
  [x, y, z, vx, vy, vz] = map read . filter (not . null) . splitOneOf ", @" $ s
