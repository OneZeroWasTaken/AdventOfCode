module Day13 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input13.txt"

  let p1       = parsePairs l
      ordering = map (uncurry compare') p1
      z        = [ n | n <- [1 .. length ordering], ordering !! (n - 1) == LT ]
  putStrLn $ "Part 1: " ++ (show . sum) z

  let p2     = map parse $ filter (not . null) l
      sorted = sort (extra ++ p2)
  putStrLn $ "Part 2: " ++ (show . product . map (signalIndex sorted)) extra
  return ()

signalIndex :: [Signal] -> Signal -> Int
signalIndex ss s = (+ 1) $ fromJust $ elemIndex s ss

data Signal = List [Signal] | Num Int
 deriving (Show, Eq)

instance Ord Signal where
  a <= b = compare' b a == GT

extra :: [Signal]
extra = [parse "[[2]]", parse "[[6]]"]

compare' :: Signal -> Signal -> Ordering
compare' (Num l) (Num r) | l < r     = LT
                         | l > r     = GT
                         | otherwise = EQ
compare' (List (l : ll)) (List (r : rr))
  | compare' l r == EQ = compare' (List ll) (List rr)
  | otherwise          = compare' l r
compare' (  List []) (  List []) = EQ
compare' (  List []) (  List _ ) = LT
compare' (  List _ ) (  List []) = GT
compare' (  Num  l ) r@(List _ ) = compare' (List [Num l]) r
compare' l@(List _ ) (  Num  r ) = compare' l (List [Num r])

parsePairs :: [String] -> [(Signal, Signal)]
parsePairs s = map (\(l : r : _) -> (parse l, parse r)) $ chunksOf 3 s

parse :: String -> Signal
parse = head . parseList

parseList :: String -> [Signal]
parseList [] = []
parseList (s : ss)
  | isDigit s = (Num . read) ds : parseList (drop (length ds) ss)
  | s == '['  = List (parseList within) : parseList (drop (length within) ss)
  | otherwise = parseList ss
 where
  ds     = takeWhile isDigit (s : ss)
  within = toMatching ss 0

toMatching :: String -> Int -> String
toMatching [] _ = error "No matching []"
toMatching (c : s) n | c == ']' && n == 0 = []
                     | c == ']'           = c : toMatching s (n - 1)
                     | c == '['           = c : toMatching s (n + 1)
                     | otherwise          = c : toMatching s n
