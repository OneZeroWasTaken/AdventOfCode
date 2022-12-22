module Day22 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input22.txt"

  let path      = parsePath (last l, R)
  let board     = padd $ take (length l - 2) l
  let start     = Pos (fromJust $ elemIndex '.' (board !! 1)) 1

  let pwd1      = password (traverse2D board path start) (last path)
  let (_, p, d) = traverse3D (parseBlocks board) path (1, Pos 1 1, R)
  let pwd2      = password (p { gx = gx p + 1, gy = gy p + 101 }) d

  putStrLn $ "Part 1: " ++ show pwd1
  putStrLn $ "Part 2: " ++ show pwd2


password :: Pos -> Dir -> Int
password p d = let facing = fromEnum d in 1000 * gy p + 4 * gx p + facing

padd :: [String] -> [String]
padd m = replicate w ' ' : map paddSides m ++ [replicate w ' ']
 where
  paddSides s = let n = length s - 1 in ' ' : s ++ replicate (w - n) ' '
  w = 1 + length (head m)

traverse2D :: [String] -> [Dir] -> Pos -> Pos
traverse2D _ [] p = p
traverse2D m (d : dirs) p | c == '.'  = traverse2D m dirs np
                          | c == '#'  = traverse2D m dirs p
                          | otherwise = traverse2D m dirs (wrap m d p)
 where
  np = p <+> d
  c  = mapPos m np

wrap :: [String] -> Dir -> Pos -> Pos
wrap m d original = tryWrap original
 where
  tryWrap :: Pos -> Pos
  tryWrap p | c == ' ' = tryWrap np
            | c == '#' = original
            | c == '.' = np
   where
    np = tp m $ p <+> d
    c  = mapPos m np
    tp :: [String] -> Pos -> Pos
    tp m p | gx p < 0                = p { gx = length (head m) - 1 }
           | gy p < 0                = p { gy = length m - 1 }
           | gx p >= length (head m) = p { gx = 0 }
           | gy p >= length m        = p { gy = 0 }
           | otherwise               = p

traverse3D :: [[String]] -> [Dir] -> (Int, Pos, Dir) -> (Int, Pos, Dir)
traverse3D _ [] np = np
traverse3D bs (d : dirs) (b, p, _)
  | outside && mapAfterFold == '.' = traverse3D bs turnedDirs (b2, foldPos, d2)
  | outside && mapAfterFold == '#' = traverse3D bs dirs (b, p, d)
  | mapBeforeFold == '.'           = traverse3D bs dirs (b, newPos, d)
  | mapBeforeFold == '#'           = traverse3D bs dirs (b, p, d)
 where
  outside                = x < 0 || y < 0 || x > 49 || y > 49
  newPos@(Pos x y)       = p <+> d
  mapBeforeFold          = boxPos bs (b, newPos)
  (b2, foldPos, d2, trn) = fold bs (b, p, d)
  mapAfterFold           = boxPos bs (b2, foldPos)
  turnedDirs             = map (turn trn) dirs

fold :: [[String]] -> (Int, Pos, Dir) -> (Int, Pos, Dir, Char)
fold bs (1, p, L) = (4, toL $ flipY p, R, 'F')
fold bs (1, p, U) = (6, toL $ swapXY p, R, 'R')
fold bs (2, p, R) = (5, toR $ flipY p, L, 'F')
fold bs (2, p, D) = (3, toR $ swapXY p, L, 'R')
fold bs (3, p, R) = (2, toD $ swapXY p, U, 'L')
fold bs (3, p, L) = (4, toU $ swapXY p, D, 'L')
fold bs (4, p, L) = (1, toL $ flipY p, R, 'F')
fold bs (4, p, U) = (3, toL $ swapXY p, R, 'R')
fold bs (5, p, R) = (2, toR $ flipY p, L, 'F')
fold bs (5, p, D) = (6, toR $ swapXY p, L, 'R')
fold bs (6, p, R) = (5, toD $ swapXY p, U, 'L')
fold bs (6, p, L) = (1, toU $ swapXY p, D, 'L')
-- Maintain direction
fold bs (1, p, R) = (2, toL p, R, '_')
fold bs (1, p, D) = (3, toU p, D, '_')
fold bs (2, p, L) = (1, toR p, L, '_')
fold bs (2, p, U) = (6, toD p, U, '_')
fold bs (3, p, D) = (5, toU p, D, '_')
fold bs (3, p, U) = (1, toD p, U, '_')
fold bs (4, p, R) = (5, toL p, R, '_')
fold bs (4, p, D) = (6, toU p, D, '_')
fold bs (5, p, L) = (4, toR p, L, '_')
fold bs (5, p, U) = (3, toD p, U, '_')
fold bs (6, p, D) = (2, toU p, D, '_')
fold bs (6, p, U) = (4, toD p, U, '_')

flipY, toL, toR, toU, toD, swapXY :: Pos -> Pos
flipY q = q { gy = 49 - gy q }
toL q = q { gx = 0 }
toR q = q { gx = 49 }
toU q = q { gy = 0 }
toD q = q { gy = 49 }
swapXY q = q { gx = gy q, gy = gx q }

mapPos :: [String] -> Pos -> Char
mapPos m p = m !! gy p !! gx p

boxPos :: [[String]] -> (Int, Pos) -> Char
boxPos bs (n, p) = bs !! (n - 1) !! gy p !! gx p

(<+>) :: Pos -> Dir -> Pos
Pos x y <+> R = Pos (x + 1) y
Pos x y <+> D = Pos x (y + 1)
Pos x y <+> L = Pos (x - 1) y
Pos x y <+> U = Pos x (y - 1)

turn :: Char -> Dir -> Dir
turn 'R' U      = R
turn 'L' R      = U
turn 'R' facing = succ facing
turn 'L' facing = pred facing
turn 'F' facing = turn 'R' $ turn 'R' facing
turn _   facing = facing

parsePath :: (String, Dir) -> [Dir]
parsePath ([], _) = []
parsePath (s, f)
  | (not . null) nrs = replicate (read nrs) f ++ parsePath (rest, f)
  | otherwise        = parsePath (tail s, turn (head s) f)
 where
  nrs  = takeWhile isDigit s
  rest = dropWhile isDigit s

parseBlocks :: [String] -> [[String]]
parseBlocks m' = [b1, b2, b3, b4, b5, b6]
 where
  m  = chunksOf 50 . drop 1 . map (filter (/= ' ')) $ m'
  b1 = map (take 50) (head m)
  b2 = map (drop 50) (head m)
  b3 = m !! 1
  b4 = map (take 50) (m !! 2)
  b5 = map (drop 50) (m !! 2)
  b6 = m !! 3

data Dir = R | D | L | U
  deriving (Eq, Show, Enum)

data Pos = Pos { gx :: Int, gy :: Int }
  deriving Eq

instance Show Pos where
  show (Pos x y) = 'P' : show (x, y)
