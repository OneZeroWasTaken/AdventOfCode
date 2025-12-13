module Day09 where

import Data.List
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input09.txt"

  let ps = map parse l
  let allRects = [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]
  putStrLn $ "Part 1: " ++ show (maximum [area p1 p2 | p1 <- ps, p2 <- ps])

  let perim = perimiter (ps ++ [head ps]) S.empty
  let sar = map snd $ reverse $ sort [(area p1 p2, (p1, p2)) | (p1, p2) <- allRects]
  putStrLn $ "Part 2: " ++ show (uncurry area $ head $ filter (noPointInside perim) {-$ drop 100000-} sar)

perimiter :: [(Int, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
perimiter ((x1, y1) : c2@(x2, y2) : cs) acc
  | x1 == x2  = perimiter (c2:cs) $ foldr S.insert acc [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | otherwise = perimiter (c2:cs) $ foldr S.insert acc [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
perimiter _ acc = acc

noPointInside :: S.Set (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
noPointInside ps (c1, c2) = S.null $ S.filter (\p -> insideArea p c1 c2) ps

insideArea :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
insideArea (x, y) (x1, y1) (x2, y2) = x > min x1 x2 && x < max x1 x2 && y > min y1 y2 && y < max y1 y2

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

parse :: String -> (Int, Int)
parse s = let [a, b] = map read (splitOn "," s) in (a, b)
