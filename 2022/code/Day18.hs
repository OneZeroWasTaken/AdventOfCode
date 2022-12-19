module Day18 where

import           Data.List
import           Data.List.Split

size :: Int
size = 24

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input18.txt"
  let ps         = map parse l
      p1Surfaces = surfaces ps
  putStrLn ("Part 1 surfaces: " ++ show p1Surfaces)

  let outerShape    = floodFillOuter (map (<+> Pos 1 1 1) ps) [Pos 0 0 0] []
      outerSurfaces = surfaces outerShape
      p2Surfaces    = outerSurfaces - 6 * size ^ 2
  putStrLn ("Part 2 surfaces: " ++ show p2Surfaces)

surfaces :: [Pos] -> Int
surfaces ps = sum $ map dimSurfaces [x, y, z]
 where
  x = sort ps
  y = sort $ map shift x
  z = sort $ map shift y

floodFillOuter :: [Pos] -> [Pos] -> [Pos] -> [Pos]
floodFillOuter s []         acc = acc
floodFillOuter s (t : todo) acc = floodFillOuter s (new ++ todo) (t : acc)
 where
  new = filter isNew . filter withinBounds . map (<+> t) $ dirs
  isNew p = p `notElem` todo && p `notElem` acc && p `notElem` s

withinBounds :: Pos -> Bool
withinBounds (Pos x y z) | x < 0 || y < 0 || z < 0             = False
                         | x >= size || y >= size || z >= size = False
                         | otherwise                           = True

(<+>) :: Pos -> Pos -> Pos
Pos x1 y1 z1 <+> Pos x2 y2 z2 = Pos (x1 + x2) (y1 + y2) (z1 + z2)

dirs :: [Pos]
dirs =
  [Pos 1 0 0, Pos (-1) 0 0, Pos 0 1 0, Pos 0 (-1) 0, Pos 0 0 1, Pos 0 0 (-1)]

shift :: Pos -> Pos
shift (Pos a b c) = Pos b c a

dimSurfaces :: [Pos] -> Int
dimSurfaces [_] = 2
dimSurfaces (p : q : ps) | touching1D p q = dimSurfaces (q : ps)
                         | otherwise      = dimSurfaces (q : ps) + 2

touching1D :: Pos -> Pos -> Bool
touching1D (Pos x1 y1 z1) (Pos x2 y2 z2)
  | x1 == x2 && y1 == y2 = abs (z2 - z1) == 1
  | otherwise            = False

parse :: String -> Pos
parse s = Pos a b c where [a, b, c] = read <$> splitOn "," s

data Pos = Pos { gx :: Int , gy :: Int , gz :: Int } deriving (Eq)

instance Show Pos where
  show p = 'P' : show (gx p, gy p, gz p)

instance Ord Pos where
  Pos x1 y1 z1 <= Pos x2 y2 z2 = (x1, y1, z1) <= (x2, y2, z2)
