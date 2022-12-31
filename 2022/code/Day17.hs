module Day17 where

import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- head . lines <$> readFile "inputs/input17.txt"
  --print $ length l


  let a = simRock l (emptyChamber, Pos 2 6) 1
  print a
  return ()

emptyChamber :: [[Bool]]
emptyChamber =
  let w = 7
      h = 15
  in  replicate h (replicate w False) ++ [replicate w True]

simRock :: String -> ([[Bool]], Pos) -> Int -> ([[Bool]], Pos)
simRock _ a 3 = a
simRock w (ch, p) i | inAir     = simRock w (ch, afDown) i
                    | otherwise = simRock w (newCh, newPos) (i + 1)
 where
  curRock = rock i
  tryWind = p + wind w i
  afWind  = if canMove ch tryWind curRock then tryWind else p
  tryDown = afWind + Pos 0 1
  inAir   = canMove ch tryDown curRock
  afDown  = if inAir then tryDown else afWind
  newCh   = setInStone ch (map (+ afDown) curRock)
  newPos  = Pos 2 ((-) 4 . fromJust . elemIndex True . map or $ newCh)

setInStone :: [[Bool]] -> [Pos] -> [[Bool]]
setInStone =
  foldl (\ch p -> replaceAt (gy p) (replaceAt (gx p) True (ch !! gy p)) ch)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

canMove :: [[Bool]] -> Pos -> [Pos] -> Bool
canMove ch p ps = all allowed allPs
 where
  allPs = map (+ p) ps
  allowed :: Pos -> Bool
  allowed (Pos x y) | x < 0 || x >= 7 = False
                    | otherwise       = not $ ch !! y !! x

wind :: String -> Int -> Pos
wind s i = case s !! i of
  '<' -> Pos (-1) 0
  '>' -> Pos 1 0
  _   -> error "Wind is wrong :("

rock :: Int -> [Pos]
rock i = rocks !! (i `mod` 5)

rocks :: [[Pos]]
rocks =
  [ [Pos 0 0, Pos 1 0, Pos 2 0, Pos 3 0]
  , [Pos 1 0, Pos 0 (-1), Pos 1 (-1), Pos 2 (-1), Pos 1 (-2)]
  , [Pos 0 0, Pos 1 0, Pos 2 0, Pos 2 (-1), Pos 2 (-2)]
  , [Pos 0 0, Pos 0 (-1), Pos 0 (-2), Pos 0 (-3)]
  , [Pos 0 0, Pos 1 0, Pos 0 (-1), Pos 1 (-1)]
  ]

data Pos = Pos { gx :: Int, gy :: Int }
 deriving Eq

instance Show Pos where
  show (Pos x y) = 'P' : show (x, y)

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
  (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
  abs (Pos x y) = Pos (abs x) (abs y)
  signum (Pos x y) = Pos (signum x) (signum y)
  fromInteger _ = Pos 0 0
  negate (Pos x y) = Pos (-x) (-y)

