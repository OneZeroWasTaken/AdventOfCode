module Day10 where

import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input10.txt"
  let regX  = cycle' 1 1 $ concatMap parse l
      part1 = sum . interesting $ zipWith (*) regX [1 ..]
      part2 = zipWith toPixel regX [ i | _ <- [1 .. 6], i <- [0 .. 39] ]
  putStrLn $ "Part 1: " ++ show part1
  putStrLn "Part 2:"
  prettyPrint part2 40
  return ()

toPixel :: Int -> Int -> Char
toPixel x i | i `elem` [x - 1 .. x + 1] = '#'
            | otherwise                 = '.'

cycle' :: Int -> Int -> [Op] -> [Int]
cycle' _ _ [] = []
cycle' c x (op : ops) | op `elem` [NOOP, NOOP_ADDX] = x : cycle' (c + 1) x ops
                      | otherwise = x : cycle' (c + 1) newX ops
 where
  (ADDX n) = op
  newX     = x + n

interesting :: [Int] -> [Int]
interesting l = [ l !! (i * 40 + 19) | i <- [0 .. 5] ]

data Op = NOOP | NOOP_ADDX | ADDX Int
    deriving (Eq, Show)

parse :: String -> [Op]
parse "noop" = [NOOP]
parse s      = [NOOP_ADDX, ADDX $ read n] where [_, n] = splitOn " " s

prettyPrint :: String -> Int -> IO ()
prettyPrint s n = if length s < n
  then return ()
  else do
    putStrLn $ take n s
    prettyPrint (drop n s) n
