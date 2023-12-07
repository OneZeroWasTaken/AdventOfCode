module Day07 where

import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  l <- map parse . lines <$> readFile "inputs/input07.txt"

  let part1 = zip (sortBy (sortHand strength cardOrder1) l) [1 ..]
  let part2 = zip (sortBy (sortHand jokerStrength cardOrder2) l) [1 ..]
  putStrLn $ "Part 1: " ++ show (winnings part1)
  putStrLn $ "Part 2: " ++ show (winnings part2)

winnings :: [(Hand, Int)] -> Int
winnings = sum . map (\((_, bid), rank) -> bid * rank)

type Hand = (String, Int)

sortHand :: (String -> Int) -> String -> Hand -> Hand -> Ordering
sortHand strFun cardOrder (h1, _) (h2, _)
  | h1str > h2str = GT
  | h1str < h2str = LT
  | otherwise     = cardStrength cardOrder h1 h2
 where
  h1str = strFun h1
  h2str = strFun h2

strength :: String -> Int
strength h | nUnique 1 h = 50
           | hasN 4 h    = 40
           | nUnique 2 h = 32
           | hasN 3 h    = 30
           | nUnique 3 h = 22
           | nUnique 4 h = 2
           | nUnique 5 h = 1
 where
  nUnique n = (== n) . length . nub
  hasN n s = (== n) . maximum . map (`countElem` s) $ s

jokerStrength :: String -> Int
jokerStrength s
  | s == "JJJJJ" = strength s
  | 'J' `notElem` s = strength s
  | otherwise = maximum
  $ map (jokerStrength . replaceFirstJ s) (cardOrder2 \\ "J")

replaceFirstJ :: String -> Char -> String
replaceFirstJ [] _ = []
replaceFirstJ (j : s) c | j == 'J'  = c : s
                        | otherwise = j : replaceFirstJ s c

cardStrength :: String -> String -> String -> Ordering
cardStrength cardOrder (c1 : h1) (c2 : h2)
  | c1 == c2  = cardStrength cardOrder h1 h2
  | fromJust (elemIndex c1 cardOrder) < fromJust (elemIndex c2 cardOrder) = LT
  | otherwise = GT

cardOrder1, cardOrder2 :: String
cardOrder1 = "23456789TJQKA"
cardOrder2 = "J23456789TQKA"

countElem :: Eq a => a -> [a] -> Int
countElem e = length . filter (e ==)

parse :: String -> (String, Int)
parse s = (c, read bid) where (c, ' ' : bid) = splitAt 5 s
