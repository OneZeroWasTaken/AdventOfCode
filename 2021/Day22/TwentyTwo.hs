module TwentyTwo where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let cuboids = parseCuboids $ lines content
  print cuboids
  partOne
  putStrLn ""
  --partTwo
  return ()

parseCuboids :: [String] -> [Cuboid]
parseCuboids = map parseCuboid
 where
  parseCuboid :: String -> Cuboid
  parseCuboid s = Cuboid state (x1, y1, z1) (x2, y2, z2)
   where
    state = if take 2 s == "on" then On else Off
    x1    = read $ takeWhile (/= '.') $ tail $ dropWhile (/= '=') s
    x2    = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= '.') s
    y1    = read $ takeWhile (/= '.') $ drop 2 $ dropWhile (/= 'y') s
    y2    = read $ takeWhile (/= ',') $ drop 2 $ dropWhile (/= '.') $ dropWhile
      (/= 'y')
      s
    z1 = read $ takeWhile (/= '.') $ drop 2 $ dropWhile (/= 'z') s
    z2 = read $ drop 2 $ dropWhile (/= '.') $ dropWhile (/= 'z') s


data Cuboid = Cuboid State (Int, Int, Int) (Int, Int, Int)
 deriving (Show, Eq)

data State = On | Off
 deriving (Show, Eq)

on :: (Int, Int, Int) -> (Int, Int, Int) -> Cuboid
on = Cuboid On

off :: (Int, Int, Int) -> (Int, Int, Int) -> Cuboid
off = Cuboid Off

isOn :: Cuboid -> Bool
isOn (Cuboid On  _ _) = True
isOn (Cuboid Off _ _) = False

size :: Cuboid -> Int
size (Cuboid _ (x1, y1, z1) (x2, y2, z2)) =
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

sizeAll :: [Cuboid] -> [Cuboid] -> Int
sizeAll [] _ = 0
sizeAll (c : cs) checked
  | isOn c    = sizeAll cs (c : checked) + compositeSize c checked
  | otherwise = sizeAll cs (c : checked) + negComposite c checked

compositeSize :: Cuboid -> [Cuboid] -> Int
compositeSize c [] = size c
compositeSize c cs =
  size c - sum (map (size . overlapSize c) (filter (isRelevant c) cs))

isRelevant :: Cuboid -> Cuboid -> Bool
isRelevant c u = isOverlapping c u

negComposite :: Cuboid -> [Cuboid] -> Int
negComposite _ [] = 0
negComposite c cs =
  -(sum $ map (size . overlapSize c) (filter (isOverlapping c) cs))
    +     special
    `div` 2
 where
  allPairs   = [ (x, y) | x <- cs, y <- cs, x /= y ]
  allOverlap = filter (\(x, y) -> isOverlapping3 x y c) allPairs
  addOverlap = map (uncurry overlap) allOverlap
  special    = sum $ map (size . overlapSize c) addOverlap


overlap :: Cuboid -> Cuboid -> Cuboid
overlap c u | isInside c u      = if sc > su then c else u
            | isOverlapping c u = overlapSize c u
            | otherwise         = on (0, 0, 0) (0, 0, 0)
 where
  sc = size c
  su = size u

isInside :: Cuboid -> Cuboid -> Bool
isInside c u = check c u || check u c
 where
  check (Cuboid _ (x1, y1, z1) (x2, y2, z2)) (Cuboid _ (x3, y3, z3) (x4, y4, z4))
    = x1 <= x3 && x4 <= x2 && y1 <= y3 && y4 <= y2 && z1 <= z3 && z4 <= z2

isOverlapping :: Cuboid -> Cuboid -> Bool
isOverlapping c u = check c u || check u c
 where
  check (Cuboid _ (x1, y1, z1) (x2, y2, z2)) (Cuboid _ (x3, y3, z3) (_, _, _))
    = x1 <= x3 && x3 <= x2 && y1 <= y3 && y3 <= y2 && z1 <= z3 && z3 <= z2

isOverlapping3 :: Cuboid -> Cuboid -> Cuboid -> Bool
isOverlapping3 a b c =
  isOverlapping a b && isOverlapping b c && isOverlapping a c

overlapSize :: Cuboid -> Cuboid -> Cuboid
overlapSize c u = calc c u
 where
  calc (Cuboid _ (x1, y1, z1) (x2, y2, z2)) (Cuboid _ (x3, y3, z3) (x4, y4, z4))
    = Cuboid On
             (max x1 x3, max y1 y3, max z1 z3)
             (min x2 x4, min y2 y4, min z2 z4)
      --min (x2 - x3 + 1) (x4 - x1 + 1) * min (y2 - y3 + 1) (y4 - y1 + 1) * min
      --(z2 - z3 + 1)
      --(z4 - z1 + 1)

partOne :: IO ()
partOne = do
  putStrLn $ "Part 1 Score = " ++ show ()
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Cubes = " ++ show ()
  return ()

