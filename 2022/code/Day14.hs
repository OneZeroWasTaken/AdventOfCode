module Day14 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input14.txt"
  let rocks = concatMap parseRocks l
  putStrLn $ "Part 1 sand: " ++ (show . length . sim []) rocks
  putStrLn
    $  "Part 2 sand: "
    ++ (show . (+ 1) . length . simOne (withInfFloor rocks) []) spawn
  return ()

simOne :: [Pos] -> [Pos] -> Pos -> [Pos]
simOne rocks acc (Pos x y) = c
 where
  dw = Pos x (y + 1)
  dl = Pos (x - 1) (y + 1)
  dr = Pos (x + 1) (y + 1)
  a  = filter (`notElem` acc) [dw, dl, dr]
  b  = filter (`notElem` rocks) a
  c  = foldl (simOne rocks) (acc ++ b) b


sim :: [Pos] -> [Pos] -> [Pos]
sim sand rocks | length newSand == length sand = sand
               | otherwise                     = sim newSand rocks
  where newSand = fall (maxY rocks) (spawn : sand) rocks

maxY :: [Pos] -> Int
maxY = maximum . map gy

spawn :: Pos
spawn = Pos 500 0

withInfFloor :: [Pos] -> [Pos]
withInfFloor rocks =
  between [Pos (gx spawn - 501) y, Pos (gx spawn + 501) y] ++ rocks
  where y = maxY rocks + 2

fall :: Int -> [Pos] -> [Pos] -> [Pos]
fall my (Pos x y : sand) rocks
  | y == my          = sand
  | dw `notElem` sand && dw `notElem` rocks = fall my (dw : sand) rocks
  | dl `notElem` sand && dl `notElem` rocks = fall my (dl : sand) rocks
  | dr `notElem` sand && dr `notElem` rocks = fall my (dr : sand) rocks
  | Pos x y == spawn = sand
  | otherwise        = Pos x y : sand
 where
  dw = Pos x (y + 1)
  dl = Pos (x - 1) (y + 1)
  dr = Pos (x + 1) (y + 1)

parseRocks :: String -> [Pos]
parseRocks s = between corners
 where
  sp = filter (not . null . head) . map (splitOn ",") . splitOneOf " ->" $ s
  corners = map (\(x : y : _) -> Pos (read x) (read y)) sp

between :: [Pos] -> [Pos]
between = nub . between'
 where
  between' :: [Pos] -> [Pos]
  between' (f : t : ps)
    | gx f == gx t
    = [ Pos (gx f) y | y <- [min (gy f) (gy t) .. max (gy f) (gy t)] ]
      ++ between' (t : ps)
    | otherwise
    = [ Pos x (gy f) | x <- [min (gx f) (gx t) .. max (gx f) (gx t)] ]
      ++ between' (t : ps)
  between' _ = []

data Pos = Pos { gx :: Int, gy :: Int }
 deriving (Show, Eq)

