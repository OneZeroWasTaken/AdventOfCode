module Day08 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input08.txt"

  let ps = map parse ss
  let es = map (\(_, p1, p2) -> (p1, p2)) (take 1000 $ edges ps)
  let vs = allV es 
  putStrLn $ "Part 1: " ++ show (sort $ map length $ map (\p -> flood es [p]) vs)
  putStrLn $ "Part 2: " ++ show ()

allV :: [((Int, Int, Int), (Int, Int, Int))] -> [(Int, Int, Int)]
allV es = nub $ map fst es ++ map snd es

flood :: [((Int, Int, Int), (Int, Int, Int))] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
flood es vs | all (`elem` vs) con = vs
            | otherwise = nub $concatMap (\p -> flood es (p:vs)) next
  where
    f = head vs
    con = nub $ map snd (filter ((== f) . fst) es) ++ map fst (filter ((== f) . snd) es)
    next = filter (`notElem` vs) con

circuits :: [((Int, Int, Int), (Int, Int, Int))] -> (Int, Int, Int) -> [(Int, Int, Int)]
circuits [] jb = [jb]
circuits ((v1, v2):es) jb = undefined

edges :: [(Int, Int, Int)] -> [(Int, (Int, Int, Int), (Int, Int, Int))]
edges ps =
  nub $
    filter (\(d, _, _) -> d > 0) $
      sort
        [(dist p1 p2, head (sort [p1, p2]), last (sort [p1, p2])) | p1 <- ps, p2 <- ps]

dist :: (Int, Int, Int) -> (Int, Int, Int) -> Int
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

parse :: String -> (Int, Int, Int)
parse s = let [a, b, c] = map read (splitOn "," s) in (a, b, c)
