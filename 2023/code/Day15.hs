module Day15 where

import           Data.List.Split
import           Data.Char
import           Data.Maybe

main :: IO ()
main = do
  l <- init <$> readFile "inputs/input15.txt"

  putStrLn $ "Part 1: " ++ show (sum . map hash . splitOn "," $ l)
  putStrLn $ "Part 2: " ++ show
    (focusingPower $ foldl part2 hashmap $ parsePart2 l)

-- Part 1:)

hash :: String -> Int
hash = foldl (\v -> (`mod` 256) . (* 17) . (+ v) . ord) 0

-- Part 2:(

data Lens = Lens String Int
 deriving (Eq, Show)

hashmap :: [(Int, [Lens])]
hashmap = [ (n, []) | n <- [0 .. 255] ]

part2 :: [(Int, [Lens])] -> Lens -> [(Int, [Lens])]
part2 hm lens@(Lens l n) | n == -1   = part2' remove
                         | otherwise = part2' add
 where
  part2' :: ([Lens] -> Lens -> [Lens]) -> [(Int, [Lens])]
  part2' f = replaceKeyWith hm (hash l) (f boxLs lens)
  boxLs = fromJust $ lookup (hash l) hm

replaceKeyWith :: [(Int, [Lens])] -> Int -> [Lens] -> [(Int, [Lens])]
replaceKeyWith [] k new = []
replaceKeyWith (kv@(key, _) : ks) k new
  | k == key  = (key, new) : ks
  | otherwise = kv : replaceKeyWith ks k new

add :: [Lens] -> Lens -> [Lens]
add [] l = [l]
add (lens@(Lens l' _) : ls) new@(Lens l _) | l' == l   = new : ls
                                           | otherwise = lens : add ls new

remove :: [Lens] -> Lens -> [Lens]
remove [] _ = []
remove (lens@(Lens l' _) : ls) rm@(Lens l _) | l' == l   = ls
                                             | otherwise = lens : remove ls rm

focusingPower :: [(Int, [Lens])] -> Int
focusingPower [] = 0
focusingPower ((bn, ls) : es) =
  focusingPower es + sum (zipWith (\(Lens _ n) i -> (bn + 1) * n * i) ls [1 ..])

parsePart2 :: String -> [Lens]
parsePart2 = map f . splitOn ","
 where
  f s = if '=' `elem` s then Lens label (read n) else Lens label (-1)
    where [label, n] = splitOneOf "-=" s
