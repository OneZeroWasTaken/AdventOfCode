{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day09 where

import Data.Char
import Data.Maybe

main :: IO ()
main = do
  l <- parse . init <$> readFile "inputs/input09.txt"
  let is = catMaybes l

  putStrLn $ "Part 1: " ++ show (checksum (zip [0 ..] $ take (length is) l) (reverse is))

checksum :: [(Int, Maybe Int)] -> [Int] -> Int
checksum [] _ = 0
checksum ((i, Just f) : fr) bk = i * f + checksum fr bk
checksum ((i, Nothing) : fr) (b : bk) = i * b + checksum fr bk

parse :: String -> [Maybe Int]
parse = concat . zipWith parseChar [0 ..]
  where
    parseChar :: Int -> Char -> [Maybe Int]
    parseChar i c
      | even i = replicate (digitToInt c) (Just (i `div` 2))
      | otherwise = replicate (digitToInt c) Nothing
