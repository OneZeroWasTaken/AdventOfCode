module Fourteen where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  let start = head input
  let table = parseTable $ drop 2 input
  partOne start table
  putStrLn ""
  partTwo start table
  return ()

parseTable :: [String] -> [(String, Char)]
parseTable = map (\s -> (take 2 s, last s))

step :: String -> [(String, Char)] -> String
step []             _ = []
step [l           ] _ = [l]
step (l : k : rest) t = case lookup [l, k] t of
  Nothing -> l : step (k : rest) t
  Just c  -> l : c : step (k : rest) t

stepN :: Int -> String -> [(String, Char)] -> String
stepN 0 i _ = i
stepN n i t = stepN (n - 1) (step i t) t

partOne :: String -> [(String, Char)] -> IO ()
partOne input table = do
  let polymer = stepN 10 input table
  let indexed = counts $ sort polymer
  let diff    = fst (maximum indexed) - fst (minimum indexed)
  putStrLn $ "Part 1 Polymer = " ++ show diff
  return ()

counts :: String -> [(Int, Char)]
counts []       = []
counts (l : ls) = (count l, l) : counts (dropWhile (== l) ls)
  where count c = 1 + length (takeWhile (== c) ls)


partTwo :: String -> [(String, Char)] -> IO ()
partTwo input table = do
  let polymer = stepN2 40 table (stringToPairs input [])
  let letters = sumLetters polymer
  let adjusted =
        increaseEntryBy (last input) 1 $ increaseEntryBy (head input) 1 letters
  let adjusted2 = map (`div` 2) $ snd $ unzip adjusted
  let diff      = maximum adjusted2 - minimum adjusted2
  putStrLn $ "Part 2 Polymer = " ++ show (diff)
  return ()

stringToPairs :: String -> [(String, Int)] -> [(String, Int)]
stringToPairs []           t = t
stringToPairs [_         ] t = t
stringToPairs (l : k : ss) t = case lookup [l, k] t of
  Nothing -> stringToPairs (k : ss) (([l, k], 1) : t)
  Just _  -> stringToPairs (k : ss) (increaseEntryBy [l, k] 1 t)

-- | Sums letters, which in this case counts all letters twice (except the first and last)
sumLetters :: [(String, Int)] -> [(Char, Int)]
sumLetters []                  = []
sumLetters (([]      , _) : r) = sumLetters r
sumLetters (((l : ls), n) : r) = combineEntry l n $ sumLetters ((ls, n) : r)

stepN2 :: Int -> [(String, Char)] -> [(String, Int)] -> [(String, Int)]
stepN2 0 _ i = i
stepN2 n t i = stepN2 (n - 1) t (step2 t i [])

step2
  :: [(String, Char)] -> [(String, Int)] -> [(String, Int)] -> [(String, Int)]
step2 _ []            acc = acc
step2 t ((p, n) : ps) acc = case lookup p t of
  Nothing -> step2 t ps acc
  Just c ->
    step2 t ps $ combineEntry [c, last p] n $ combineEntry [head p, c] n acc

combineEntry :: Eq a => a -> Int -> [(a, Int)] -> [(a, Int)]
combineEntry s n t = case lookup s t of
  Nothing -> (s, n) : t
  Just _  -> increaseEntryBy s n t

increaseEntryBy :: Eq a => a -> Int -> [(a, Int)] -> [(a, Int)]
increaseEntryBy _ _ [] = []
increaseEntryBy entry by ((e, i) : es)
  | e == entry = (e, i + by) : es
  | otherwise  = (e, i) : increaseEntryBy entry by es

