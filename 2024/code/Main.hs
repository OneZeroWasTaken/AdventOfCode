{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Day01 as D1
import Day02 as D2
import Day04 as D4
import Day05 as D5
import Day06 as D6
import Day07 as D7
import Day08 as D8
import Day09 as D9
import Day10 as D10
import System.Environment (getArgs)
import System.Process (callCommand)

-- Usage:
-- > cabal run AdventOfCode2024 -- <day>

main :: IO ()
main = getArgs >>= runDay

runDay :: [String] -> IO ()
runDay args = do
  case args of
    ["1"] -> D1.main
    ["2"] -> D2.main
    ["3"] -> putStrLn "Day 3 was done with Vim keystrokes"
    ["4"] -> D4.main
    ["5"] -> D5.main
    ["6"] -> D6.main
    ["7"] -> D7.main
    ["8"] -> D8.main
    ["9"] -> D9.main >> callCommand "dotnet run --project code -- 9"
    ["10"] -> D10.main
    [] -> do
      putStrLn "Enter day to run"
      s <- getLine
      runDay [s]
    (d : _) ->
      if read d `elem` [1 .. 25]
        then putStrLn "Day not implemented yet"
        else putStrLn "Invalid argument"
