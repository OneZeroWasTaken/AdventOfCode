module Day17 where

import Data.Bits
import Data.Char
import Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input17.txt"

  let ops = map read . splitOn "," . drop 9 . (!! 4) $ l
      aReg = read . drop 12 . head $ l

  putStrLn $ "Part 1: " ++ show (run ops aReg)
  putStrLn $ "Part 2: " ++ show (minimum $ solve ops (reverse ops) "")

data Reg = Reg {a :: Int, b :: Int, c :: Int, ops :: [Int], out :: [Int]}

binary :: String -> Int
binary = foldr (\n acc -> n + 2 * acc) 0 . reverse . map digitToInt

solve :: [Int] -> [Int] -> String -> [Int]
solve _ [] s = [binary s]
solve ops (n : ns) s =
  concatMap (solve ops ns . (s ++) . snd) $
    filter ((== n) . head . fst) $
      zip (map (run ops . binary . (s ++)) bits) bits

bits :: [String]
bits = ["111", "110", "101", "100", "011", "010", "001", "000"]

run :: [Int] -> Int -> [Int]
run ops a = reverse $ out $ inst (Reg a 0 0 ops []) ops

inst :: Reg -> [Int] -> Reg
inst r [] = r
inst r (0 : c : opcs) = inst r {a = a r `div` (2 ^ combo r c)} opcs
inst r (1 : c : opcs) = inst r {b = b r `xor` c} opcs
inst r (2 : c : opcs) = inst r {b = combo r c `mod` 8} opcs
inst r (3 : c : opcs)
  | a r == 0 = inst r opcs
  | otherwise = inst r (drop c $ ops r)
inst r (4 : _ : opcs) = inst r {b = b r `xor` c r} opcs
inst r (5 : c : opcs) = inst r {out = combo r c `mod` 8 : out r} opcs
inst r (6 : c : opcs) = inst r {b = a r `div` (2 ^ combo r c)} opcs
inst r (7 : c : opcs) = inst r {c = a r `div` (2 ^ combo r c)} opcs
inst _ _ = error "Invalid opcode"

combo :: Reg -> Int -> Int
combo r 4 = a r
combo r 5 = b r
combo r 6 = c r
combo _ n = n
