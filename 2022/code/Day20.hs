module Day20 where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence                 as S
import           Data.Tuple

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input20.txt"

  let seq1    = S.fromList $ zip (map read l) [0 ..]
  let seq2    = S.fromList $ zip (map ((* key) . read) l) [0 ..]

  let mixed   = foldl mixN seq1 [0]
  let mixed10 = foldl mixN seq2 [0 .. 9]
  putStrLn $ "Part 1: " ++ (show . sum . groveCoords) mixed
  putStrLn $ "Part 2: " ++ (show . sum . groveCoords) mixed10

mixN :: S.Seq (Int, Int) -> Int -> S.Seq (Int, Int)
mixN seq _ = foldl mix seq [0 .. S.length seq - 1]

key :: Int
key = 811589153

groveCoords :: S.Seq (Int, Int) -> [Int]
groveCoords seq =
  let zi = fromJust $ S.findIndexL ((== 0) . fst) seq
  in  map (fst . S.index seq . (`mod` S.length seq) . (+ zi)) [1000, 2000, 3000]

mix :: S.Seq (Int, Int) -> Int -> S.Seq (Int, Int)
mix seq iter = move seq i e
 where
  i = fromJust $ S.findIndexL ((== iter) . snd) seq
  e = S.index seq i

move :: S.Seq (Int, Int) -> Int -> (Int, Int) -> S.Seq (Int, Int)
move seq i e = S.insertAt ni e (S.deleteAt i seq)
 where
  ni' = (fst e + i) `mod` (S.length seq - 1)
  ni  = if ni' == 0 then S.length seq - 1 else ni'

