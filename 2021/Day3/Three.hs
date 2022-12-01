module Three where

import           Data.Char

main :: IO ()
main = do
  content <- readFile "input.txt"
  let l = lines content
  partOne l
  putStrLn ""
  partTwo l
  return ()

partOne :: [String] -> IO ()
partOne l = do
  let g  = gamma l
  let e  = epsilon g
  let vg = binListToDec g
  let ve = binListToDec e

  putStrLn $ "Part one gamma:   " ++ show g ++ " " ++ show vg
  putStrLn $ "Part one epsilon: " ++ show e ++ " " ++ show ve
  putStrLn $ "Part one answer:  " ++ show (vg * ve)

  return ()


partTwo :: [String] -> IO ()
partTwo l = do
  let o  = head $ oxygen l 0
  let vo = binListToDec $ map digitToInt o
  let s  = head $ scrubber l 0
  let vs = binListToDec $ map digitToInt s
  putStrLn $ "Part two oxygen:   " ++ show o ++ " " ++ show vo
  putStrLn $ "Part two scrubber: " ++ show s ++ " " ++ show vs
  putStrLn $ "Part two answer:   " ++ show (vo * vs)
  return ()

scrubber :: [String] -> Int -> [String]
scrubber [e] _  = [e]
scrubber es  12 = es
scrubber es i =
  scrubber (removeIf es (intToChar $ mostCommon es i (0, 0) (>)) i []) (i + 1)

oxygen :: [String] -> Int -> [String]
oxygen [e] _  = [e]
oxygen es  12 = es
oxygen es i =
  oxygen (removeIf es (intToChar $ mostCommon es i (0, 0) (<=)) i []) (i + 1)

removeIf :: [String] -> Char -> Int -> [String] -> [String]
removeIf [] _ _ acc = acc
removeIf (e : es) c i acc | e !! i == c = removeIf es c i (e : acc)
                          | otherwise   = removeIf es c i acc

intToChar :: Int -> Char
intToChar i = head $ show i

epsilon :: [Int] -> [Int]
epsilon l = [ abs $ n - 1 | n <- l ]

gamma :: [String] -> [Int]
gamma l = [ mostCommon l n (0, 0) (<=) | n <- [0 .. length (head l) - 1] ]


mostCommon :: [String] -> Int -> (Int, Int) -> (Int -> Int -> Bool) -> Int
mostCommon [] _ (z, o) c | z `c` o   = 1
                         | otherwise = 0
mostCommon (e : es) i (z, o) c
  | digitToInt (e !! i) == 0 = mostCommon es i (z + 1, o) c
  | otherwise                = mostCommon es i (z, o + 1) c

binListToDec :: [Int] -> Int
binListToDec l = sum [ (l !! (c - p)) * (2 ^ p) | p <- [0 .. c] ]
  where c = length l - 1

