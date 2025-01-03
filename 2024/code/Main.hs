{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import           Day01                         as D1
import           Day02                         as D2
import           Day04                         as D4
import           Day05                         as D5
import           Day06                         as D6
import           Day07                         as D7
import           Day08                         as D8
import           Day09                         as D9
import           Day10                         as D10
import           Day11                         as D11
import           Day12                         as D12
import           Day13                         as D13
import           Day14                         as D14
import           Day15                         as D15
import           Day16                         as D16
import           Day17                         as D17
import           Day18                         as D18
import           Day19                         as D19
import           Day20                         as D20
import           Day21                         as D21
import           Day22                         as D22
import           Day23                         as D23
import           Day24                         as D24
import           Day25                         as D25
import           System.Environment             ( getArgs )
import           System.Process                 ( callCommand )

-- Usage:
-- > cabal run AdventOfCode2024 -- <day>

main :: IO ()
main = getArgs >>= runDay

runDay :: [String] -> IO ()
runDay args = do
  case args of
    ["1" ] -> D1.main
    ["2" ] -> D2.main
    ["3" ] -> putStrLn "Day 3 was done with Vim keystrokes"
    ["4" ] -> D4.main
    ["5" ] -> D5.main
    ["6" ] -> D6.main
    ["7" ] -> D7.main
    ["8" ] -> D8.main
    ["9" ] -> D9.main >> callCommand "dotnet run --project code -- 9"
    ["10"] -> D10.main
    ["11"] -> D11.main
    ["12"] -> D12.main
    ["13"] -> D13.main
    ["14"] -> D14.main
    ["15"] -> D15.main >> callCommand "dotnet run --project code -- 15"
    ["16"] -> D16.main
    ["17"] -> D17.main
    ["18"] -> D18.main
    ["19"] -> D19.main
    ["20"] -> D20.main
    ["21"] -> D21.main
    ["22"] -> D22.main
    ["23"] -> D23.main
    ["24"] -> D24.main
    ["25"] -> D25.main
    []     -> do
      putStrLn "Enter day to run"
      s <- getLine
      runDay [s]
    (d : _) -> if read d `elem` [1 .. 25]
      then putStrLn "Day not implemented yet"
      else putStrLn "Invalid argument"
