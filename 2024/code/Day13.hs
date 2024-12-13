{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day13 where

import Data.List.Split
import Data.Maybe
import Data.Tuple

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input13.txt"

  let ms = map (parse . concat) (chunksOf 4 l)
  putStrLn $ "Part 1: " ++ show (sum . map tokens . mapMaybe solveMachine $ ms)
  putStrLn $ "Part 2: " ++ show (sum . map tokens . mapMaybe (solveMachine . largePrice) $ ms)

tokens :: (Int, Int) -> Int
tokens = uncurry (+) . fmap (* 3) . swap

largePrice :: [Int] -> [Int]
largePrice [a, b, c, d, x, y] = let extra = 10000000000000 in [a, b, c, d, x + extra, y + extra]

solveMachine :: [Int] -> Maybe (Int, Int)
solveMachine [a, b, c, d, pX, pY]
  | isIntEnough x && isIntEnough y = Just (round x, round y)
  | otherwise = Nothing
  where
    det = fromIntegral $ a * d - b * c
    x = fromIntegral (pX * d - pY * c) / det
    y = fromIntegral (a * pY - b * pX) / det

isIntEnough :: (RealFrac b) => b -> Bool
isIntEnough x = let n = 3 in round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0

parse :: String -> [Int]
parse = map read . filter (not . null) . splitOneOf "Button A:X+Y,Prize="
