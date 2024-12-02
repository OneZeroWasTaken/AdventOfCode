{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           System.Environment             ( getArgs )
import           Day01                         as D1
import           Day02                         as D2

-- Usage:
-- > cabal run AdventOfCode2024 -- <day>

main :: IO ()
main = getArgs >>= runDay

runDay :: [String] -> IO ()
runDay args = do
  case args of
    ["1"] -> D1.main
    ["2"] -> D2.main
    []    -> do
      putStrLn "Enter day to run"
      s <- getLine
      runDay [s]
    (d : _) -> if read d `elem` [1 .. 25]
      then putStrLn "Day not implemented yet"
      else putStrLn "Invalid argument"
