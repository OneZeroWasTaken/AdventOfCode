module Day24 where

import qualified Data.Set                      as S

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input24.txt"

  let bs    = parseBlizzards l
  let trip1 = sim end bs (S.fromList [start]) 0
  let trip2 = sim start (fst trip1) (S.fromList [end]) 1
  let trip3 = sim end (fst trip2) (S.fromList [start]) 1

  putStrLn $ "Part 1: " ++ (show . snd) trip1
  putStrLn $ "Part 2: " ++ (show . sum . map snd) [trip1, trip2, trip3]

sim :: Pos -> [Blizzard] -> S.Set Pos -> Int -> ([Blizzard], Int)
sim end' bs ps n | atEnd     = (nbs, n)
                 | otherwise = sim end' nbs (S.fromList nps) (n + 1)
 where
  atEnd = end' `elem` ps
  nbs   = map blow bs
  moves = filter inside $ concatMap (\p -> map (`step` p) dirs) ps
  nps   = filter (\p -> not (any (inBlizzard p) nbs)) moves

inBlizzard :: Pos -> Blizzard -> Bool
inBlizzard p (Blizzard b _) = p `S.member` b

inside :: Pos -> Bool
inside p@(Pos x y) | p == start || p == end       = True
                   | x <= 0 || y <= 0             = False
                   | x >= gx size || y >= gy size = False
                   | otherwise                    = True

blow :: Blizzard -> Blizzard
blow (Blizzard set d) = Blizzard (S.map (move d) set) d

move :: Dir -> Pos -> Pos
move d p | gx np <= 0       = np { gx = gx size - 1 }
         | gy np <= 0       = np { gy = gy size - 1 }
         | gx np >= gx size = np { gx = 1 }
         | gy np >= gy size = np { gy = 1 }
         | otherwise        = np
  where np = step d p

parseBlizzards :: [String] -> [Blizzard]
parseBlizzards l =
  [ Blizzard (S.fromList $ parsePositions l '<' (Pos 0 0)) L
  , Blizzard (S.fromList $ parsePositions l '>' (Pos 0 0)) R
  , Blizzard (S.fromList $ parsePositions l '^' (Pos 0 0)) U
  , Blizzard (S.fromList $ parsePositions l 'v' (Pos 0 0)) D
  ]

parsePositions :: [String] -> Char -> Pos -> [Pos]
parsePositions [] _ _ = []
parsePositions (s : ss) c p
  | null s      = parsePositions ss c (step D p { gx = 0 })
  | head s == c = p : parsePositions (tail s : ss) c (step R p)
  | otherwise   = parsePositions (tail s : ss) c (step R p)

step :: Dir -> Pos -> Pos
step R    p = p { gx = gx p + 1 }
step D    p = p { gy = gy p + 1 }
step L    p = p { gx = gx p - 1 }
step U    p = p { gy = gy p - 1 }
step Stay p = p

dirs :: [Dir]
dirs = [R, D, L, U, Stay]

start, end, size :: Pos
start = Pos 1 0
end = Pos 120 26
size = Pos 121 26

data Blizzard = Blizzard (S.Set Pos) Dir
 deriving Show

data Dir = R | D | L | U | Stay
 deriving (Eq, Show)

data Pos = Pos { gx :: Int, gy :: Int }
 deriving Eq

instance Ord Pos where
  Pos x1 y1 <= Pos x2 y2 = (x1, y1) <= (x2, y2)

instance Show Pos where
  show (Pos x y) = 'P' : show (x, y)
