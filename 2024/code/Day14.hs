{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid reverse" #-}
module Day14 where

import Codec.Compression.GZip (compress)
import Data.ByteString.Internal
import Data.ByteString.Lazy qualified as BL
import Data.List
import Data.List.Split

data Robot = Robot (Int, Int) (Int, Int)
  deriving (Eq, Show)

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input14.txt"

  let robs = map parse l
      hundred = map ((!! 100) . iterate (teleport . step)) robs
  putStrLn $ "Part 1: " ++ show (multiplyQuadrants hundred)

  let ps = map mapPos (allIterations robs)
      bs = map positionsToBoolList ps

  -- Accidentally implemented 3 solutions to part 2 because I had off by one error in all of them first :)
  putStrLn $
    "Part 2: "
      ++ show
        (snd . maximum . sort . zip (map entropy bs) $ [0 ..])
  putStrLn $
    "Part 2: "
      ++ show
        (snd . minimum $ zip (map approxKolmogorov bs) [0 ..])
  putStrLn $
    "Part 2: "
      ++ show
        (snd . head . filter (or . fst) $ zip (map (map and . divvy 20 1) bs) [0 ..])
  printR $ map ((!! 6377) . iterate (teleport . step)) robs

positionsToBoolList :: [(Int, Int)] -> [Bool]
positionsToBoolList ps =
  [(x, y) `elem` ps | x <- [0 .. fst bounds - 1], y <- [0 .. snd bounds - 1]]

allIterations :: [Robot] -> [[Robot]]
allIterations rs =
  scanl (\r' _ -> map (teleport . step) r') rs [0 .. uncurry (*) bounds]

multiplyQuadrants :: [Robot] -> Int
multiplyQuadrants rs =
  product $
    map (\q -> length $ filter (inQuadrant q) rs) quadrants
  where
    quadrants :: [[(Int, Int)]]
    quadrants =
      [ [(x, y) | x <- [0 .. 49], y <- [0 .. 50]],
        [(x, y) | x <- [51 .. 100], y <- [0 .. 50]],
        [(x, y) | x <- [0 .. 49], y <- [52 .. 102]],
        [(x, y) | x <- [51 .. 100], y <- [52 .. 102]]
      ]

approxKolmogorov :: [Bool] -> Int
approxKolmogorov bools =
  let serialized = BL.pack (map (\b -> if b then c2w '.' else c2w '#') bools)
   in fromEnum $ BL.length (compress serialized)

entropy :: [Bool] -> Double
entropy bools =
  let total = fromIntegral (length bools) :: Double
      countTrue = fromIntegral (length (filter id bools)) :: Double
      p1 = countTrue / total
      p0 = 1 - p1
      entropyTerm p = if p == 0 then 0 else -(p * logBase 2 p)
   in entropyTerm p1 + entropyTerm p0

inQuadrant :: [(Int, Int)] -> Robot -> Bool
inQuadrant ps (Robot p _) = p `elem` ps

teleport :: Robot -> Robot
teleport r@(Robot (x, y) s)
  | x < 0 = teleport $ Robot (x + fst bounds, y) s
  | y < 0 = teleport $ Robot (x, y + snd bounds) s
  | x >= fst bounds = teleport $ Robot (x - fst bounds, y) s
  | y >= snd bounds = teleport $ Robot (x, y - snd bounds) s
  | otherwise = r

step :: Robot -> Robot
step (Robot (x, y) (sx, sy)) = Robot (x + sx, y + sy) (sx, sy)

mapPos :: [Robot] -> [(Int, Int)]
mapPos = map (\(Robot p _) -> p)

printR :: [Robot] -> IO ()
printR rs = putStrLn (robotsToString (mapPos rs))

robotsToString :: [(Int, Int)] -> String
robotsToString rs = line 0 0
  where
    line x y
      | y == snd bounds = []
      | x == fst bounds = '\n' : line 0 (y + 1)
      | (x, y) `elem` rs = '#' : line (x + 1) y
      | otherwise = '.' : line (x + 1) y

bounds :: (Int, Int)
bounds = (101, 103)

parse :: String -> Robot
parse s =
  let [x, y, sx, sy] = map read . filter (not . null) . splitOneOf "pv=," $ s
   in Robot (x, y) (sx, sy)
