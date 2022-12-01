module Eighteen where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = map parsePair (lines content)
  let
    c
      = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"
  let input2 = map parsePair (lines c)
  partOne input2
  putStrLn ""
  --partTwo
  return ()

data Pair = P Pair Pair | N Int
 deriving Eq

instance Show Pair where
  show = showPair

showPair :: Pair -> String
showPair (N n    ) = show n
showPair (P p1 p2) = '[' : showPair p1 ++ ',' : showPair p2 ++ "]"

parsePair :: String -> Pair
parsePair s = P pa pb
 where
  (a, b) = leftRight s
  ha     = head a
  hb     = head b
  pa     = if isDigit ha then N (digitToInt ha) else parsePair a
  pb     = if isDigit hb then N (digitToInt hb) else parsePair b

leftRight :: String -> (String, String)
leftRight ('[' : s) | isDigit $ head s = (take 1 s, init $ drop 2 s)
                    | otherwise = (toMatchingBracket s, afterMatchingBracket s)
leftRight _ = error "Incorrect brackets"

afterMatchingBracket :: String -> String
afterMatchingBracket s | head s == '[' = init $ drop (match s 1 1 + 1) s
                       | otherwise     = error "Incorrect brackets"

toMatchingBracket :: String -> String
toMatchingBracket s | head s == '[' = take (match s 1 1) s
                    | otherwise     = error "Incorrect brackets"

match :: String -> Int -> Int -> Int
match _ i 0 = i
match s i n | s !! i == '[' = match s (i + 1) (n + 1)
            | s !! i == ']' = match s (i + 1) (n - 1)
            | otherwise     = match s (i + 1) n

j, k, l, m :: Pair
j = parsePair "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
k = parsePair "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
l = parsePair "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
m = parsePair "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"


explode :: Pair -> Pair
explode p | isExploding p = undefined
          | otherwise     = p

isExploding :: Pair -> Bool
isExploding p = expDepth p > 4

expDepth :: Pair -> Int
expDepth (N _    ) = 0
expDepth (P p' q') = max (1 + expDepth p') (1 + expDepth q')

val :: Pair -> Int
val (N n  ) = n
val (P p q) = val p + val q

nil :: Pair
nil = N 0

num :: Pair -> Pair -> Pair
num a b = N (val a + val b)

adR :: Pair -> Pair -> Pair
adR (P _ _) _         = error "First argument should be N"
adR n       (P le ri) = P (adR n le) ri
adR (N n2)  (N n1   ) = N (n1 + n2)

adL :: Pair -> Pair -> Pair
adL _         (P _ _) = error "Second argument should be N"
adL (P le ri) n       = P le (adL ri n)
adL (N n1   ) (N n2)  = N (n1 + n2)

brr :: Pair -> Pair
brr (P (P (P (P (P _ b) c) d) e) f) = P (P (P (P nil (adR b c)) d) e) f         -- [[[[[1,1],n],n],n],n]
brr (P (P (P (P a (P b c)) d) e) f) = P (P (P (P (adL a b) nil) (adR c d)) e) f -- [[[[n,[1,1]],n],n],n]
brr (P (P (P a (P (P b c) d)) e) f) = P (P (P (adL a b) (P nil (adR c d))) e) f -- [[[n,[[1,1],n]],n],n]
brr (P (P a (P (P (P b c) d) e)) f) = P (P (adL a b) (P (P nil (adR c d)) e)) f -- [[n,[[[1,1],n],n]],n]
brr (P a (P (P (P (P b c) d) e) f)) = P (adL a b) (P (P (P nil (adR c d)) e) f) -- [n,[[[[1,1],n],n],n]]
brr (P a (P b (P (P (P c d) e) f))) = P a (P (adL b c) (P (P nil (adR d e)) f)) -- [n,[n,[[[1,1],n],n]]]
brr (P a (P (P b (P (P c d) e)) f)) = P a (P (P (adL b c) (P nil (adR d e))) f) -- [n,[[n,[[1,1],n]],n]]
brr (P a (P (P (P b (P c d)) e) f)) = P a (P (P (P (adL b c) nil) (adR d e)) f) -- [n,[[[n,[1,1]],n],n]]
brr (P (P a (P b (P (P c d) e))) f) = P (P a (P (adL b c) (P nil (adR d e)))) f -- [[n,[n,[[1,1],n]]],n]
brr (P (P a (P (P b (P c d)) e)) f) = P (P a (P (P (adL b c) nil) (adR d e))) f -- [[n,[[n,[1,1]],n]],n]
brr (P (P (P a (P b (P c d))) e) f) = P (P (P a (P (adL b c) nil)) (adR d e)) f -- [[[n,[n,[1,1]]],n],n]
brr (P (P a (P b (P c (P d e)))) f) = P (P a (P b (P (adL c d) nil))) (adR e f) -- [[n,[n,[n,[1,1]]]],n]
brr (P a (P (P b (P c (P d e))) f)) = P a (P (P b (P (adL c d) nil)) (adR e f)) -- [n,[[n,[n,[1,1]]],n]]
brr (P a (P b (P (P c (P d e)) f))) = P a (P b (P (P (adL c d) nil) (adR e f))) -- [n,[n,[[n,[1,1]],n]]]
brr (P a (P b (P c (P (P d e) f)))) = P a (P b (P (adL c d) (P nil (adR e f)))) -- [n,[n,[n,[[1,1],n]]]]
brr (P a (P b (P c (P d (P e _))))) = P a (P b (P c (P (adL d e) nil)))         -- [n,[n,[n,[n,[1,1]]]]]
brr p = p

splitPair :: Pair -> Pair
splitPair (P le ri) | splittable le = P (splitPair le) ri
                    | otherwise     = P le (splitPair ri)
splitPair (N n) | even n    = P (N h) (N h)
                | otherwise = P (N h) (N (h + 1))
  where h = n `div` 2

splittable :: Pair -> Bool
splittable (N n    ) = n >= 10
splittable (P le ri) = splittable le || splittable ri

(++++) :: Pair -> Pair -> Pair
p ++++ q = fix $ P p q

fix :: Pair -> Pair
fix p | isExploding p = fix (brr p)
      | splittable p  = fix (splitPair p)
      | otherwise     = p

sumPairs :: Pair -> [Pair] -> Pair
sumPairs = foldl (++++)

partOne :: [Pair] -> IO ()
partOne pairs = do
  let pair = sumPairs (head pairs) (tail pairs)
  putStrLn $ "Part 1 Pair = " ++ show (pair)
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Distinct Velocities = " ++ show ()
  return ()


