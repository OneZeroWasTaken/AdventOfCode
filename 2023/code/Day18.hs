module Day18 where

import           Data.List.Split
import           Data.Char
import           Numeric

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input18.txt"

  let (part1, part2) = unzip $ map parse l
  putStrLn $ "Part 1: " ++ show (area part1)
  putStrLn $ "Part 2: " ++ show (area part2)

area :: [(Dir, Int)] -> Int
area ds = picksTheorem (shoelace cs) (perimiter cs)
  where cs = findCorners (0, 0) ds

picksTheorem :: Int -> Int -> Int
picksTheorem i b = i + b `div` 2 + 1

perimiter :: [Pos] -> Int
perimiter [p1] = uncurry (+) p1
perimiter (p1 : p2 : ps) =
  abs (fst p2 - fst p1 + snd p2 - snd p1) + perimiter (p2 : ps)

shoelace :: [Pos] -> Int
shoelace ps = (`div` 2) . abs . sum $ terms
 where
  terms = zipWith (\(x1, y1) (x2, y2) -> (y1 + y2) * (x1 - x2))
                  ps
                  (tail ps ++ [head ps])

findCorners :: Pos -> [(Dir, Int)] -> [Pos]
findCorners _ []              = []
findCorners p ((o@R, n) : es) = p : findCorners (p <+> (n, 0)) es
findCorners p ((o@D, n) : es) = p : findCorners (p <+> (0, n)) es
findCorners p ((o@L, n) : es) = p : findCorners (p <+> (-n, 0)) es
findCorners p ((o@U, n) : es) = p : findCorners (p <+> (0, -n)) es

data Dir = R | D | L | U
 deriving (Show, Read, Eq, Ord)

type Pos = (Int, Int)

(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

parse :: String -> ((Dir, Int), (Dir, Int))
parse s = ((read d1, read n1), (d2, n2))
 where
  [d1, n1, c] = splitOn " " s
  n2          = fst . head . readHex . init . filter (`notElem` "(#)") $ c
  d2          = read . (!!) ["R", "D", "L", "U"] . digitToInt . last . init $ c
