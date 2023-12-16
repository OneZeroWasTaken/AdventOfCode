module Day16 where

import qualified Data.Set                      as S
import           Data.List
import           Data.Char
import           Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input16.txt"

  let start = reflect (head (head ss)) (0, 0, E)

  putStrLn $ "Part 1: " ++ show (beams ss start)
  putStrLn $ "Part 2: " ++ show (maximum $ map (beams ss) (starts ss))

beams :: [String] -> Pos -> Int
beams ss start = length $ S.map (\(x, y, _) -> (x, y)) ps
  where ps = beams' ss [start] (S.fromList [start])

beams' :: [String] -> [Pos] -> S.Set Pos -> S.Set Pos
beams' ss [] acc = acc
beams' ss (b : bs) acc
  | (not . withinBounds ss) new = beams' ss bs acc
  | new `elem` acc = beams' ss bs acc
  | c `elem` "/\\" = beams' ss (reflect c new : bs) (S.insert new acc)
  | c == '|' = beams' ss ((x, y, S) : (x, y, N) : bs) (S.insert new acc)
  | c == '-' = beams' ss ((x, y, W) : (x, y, E) : bs) (S.insert new acc)
  | otherwise = beams' ss (new : bs) (S.insert new acc)
 where
  new@(x, y, d) = move b
  c             = ss !! y !! x

starts :: [String] -> [Pos]
starts ss =
  map (\p@(x, y, d) -> reflect (ss !! y !! x) p)
    $  sx 0               S
    ++ sx (length ss - 1) N
    ++ sy 0                      E
    ++ sy (length (head ss) - 1) W
 where
  sx y d = [ (x, y, d) | x <- [0 .. length (head ss) - 1] ]
  sy x d = [ (x, y, d) | y <- [0 .. length ss - 1] ]

reflect :: Char -> Pos -> Pos
reflect '/'  (x, y, N) = (x, y, E)
reflect '/'  (x, y, E) = (x, y, N)
reflect '/'  (x, y, S) = (x, y, W)
reflect '/'  (x, y, W) = (x, y, S)
reflect '\\' (x, y, N) = (x, y, W)
reflect '\\' (x, y, E) = (x, y, S)
reflect '\\' (x, y, S) = (x, y, E)
reflect '\\' (x, y, W) = (x, y, N)
reflect _    p         = p

withinBounds :: [String] -> Pos -> Bool
withinBounds ss (x, y, _) =
  x >= 0 && x < length (head ss) && y >= 0 && y < length ss

move :: Pos -> Pos
move (x, y, N) = (x, y - 1, N)
move (x, y, E) = (x + 1, y, E)
move (x, y, S) = (x, y + 1, S)
move (x, y, W) = (x - 1, y, W)

type Pos = (Int, Int, Dir)

data Dir = N | E | S | W
 deriving (Ord, Eq, Show)

