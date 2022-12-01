module TwentyFour where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  partOne
  putStrLn ""
  --partTwo
  return ()

data Input = Variable Var | Numeric Int
 deriving (Show, Eq)

data Var = W | X | Y | Z
 deriving (Show, Eq)

type State = ((Var, Int), (Var, Int), (Var, Int), (Var, Int))

state :: State
state = ((W, 0), (X, 0), (Y, 0), (Z, 0))

input :: [[Int]]
input =
  [ [a, b, c, d, e, f, g, h, i, j, k, l, m, n]
  | a <- li
  , b <- li
  , c <- li
  , d <- li
  , e <- li
  , f <- li
  , g <- li
  , h <- li
  , i <- li
  , j <- li
  , k <- li
  , l <- li
  , m <- li
  , n <- li
  ]
  where li = [9, 8 .. 1]


partOne :: IO ()
partOne = do
  putStrLn $ "Part 1 Score = " ++ show ()
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Cubes = " ++ show ()
  return ()


