module Day25 where

import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input25.txt"

  let snafu = map fromSnafu l
  putStrLn $ "Part 1: " ++ (show . toSnafu . sum) snafu

toSnafu :: Integer -> String
toSnafu = reverse . snafu' . reverse . (0 :) . map digitToInt . show . base5
 where
  snafu' :: [Int] -> String
  snafu' [0         ] = []
  snafu' (3 : n : ns) = '=' : snafu' (n + 1 : ns)
  snafu' (4 : n : ns) = '-' : snafu' (n + 1 : ns)
  snafu' (5 : n : ns) = '0' : snafu' (n + 1 : ns)
  snafu' (n     : ns) = intToDigit n : snafu' ns

base5 :: Integer -> Integer
base5 n | n <= 0    = 0
        | otherwise = base5 (n - p5) + p10
 where
  ts = reverse [1 .. 25]
  (p5, p10) =
    head [ (pw5 * t, pw10 * t) | (pw5, pw10) <- pws, t <- ts, pw5 * t <= n ]
  pws = reverse $ take 25 [ (5 ^ pw, 10 ^ pw) | pw <- [0 ..] ]

fromSnafu :: String -> Integer
fromSnafu s =
  let z = zip (reverse s) [0 ..] in sum [ toBase5 c * 5 ^ p | (c, p) <- z ]

toBase5 :: Char -> Integer
toBase5 '=' = -2
toBase5 '-' = -1
toBase5 '0' = 0
toBase5 '1' = 1
toBase5 '2' = 2
