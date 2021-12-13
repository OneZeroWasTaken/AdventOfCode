module Twelve where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input       = lines content
  let connections = parseConnections input
  partOne connections
  putStrLn ""
  partTwo connections
  return ()

data Connection = Connection String String
    deriving (Show, Eq)

isIn :: String -> Connection -> Bool
isIn s (Connection a b) = s == a || s == b

conA :: Connection -> String
conA (Connection a _) = a

conB :: Connection -> String
conB (Connection _ b) = b

parseConnections :: [String] -> [Connection]
parseConnections =
  map (\s -> Connection (takeWhile (/= '-') s) (tail $ dropWhile (/= '-') s))


rjFirst :: [Connection] -> [String] -> Bool -> String -> Int
rjFirst con been double now
  | now == "end"
  = 1
  | (double && all isLower now) || now == "start"
  = sum $ map (rjFirst con (now : been) double)
              (allAccessible con (delete now been) now)
  | all isLower now
  = sum (map (rjFirst con (now : been) double) (allAccessible con been now))
    + sum
        (map (rjFirst con (now : been) True)
             (delete "start" $ intersect been (allAccessible con [] now))
        )
  | double
  = sum
    (map (rjFirst con been double) (allAccessible con (delete now been) now))
  | otherwise
  = sum
      (map (rjFirst con been double) (allAccessible con (delete now been) now))
    + sum
        (map (rjFirst con been True)
             (delete "start" $ intersect been (allAccessible con [] now))
        )

thFirst :: [Connection] -> [String] -> String -> Int
thFirst con been now
  | now == "end" = 1
  | all isLower now = sum
  $ map (thFirst con (now : been)) (allAccessible con been now)
  | otherwise = sum $ map (thFirst con been) (allAccessible con been now)


allAccessible :: [Connection] -> [String] -> String -> [String]
allAccessible con been from = map
  (\c -> if conA c == from then conB c else conA c)
  cs
 where
  cs = filter (\c -> conA c `notElem` been && conB c `notElem` been)
    $ filter (isIn from) con

partOne :: [Connection] -> IO ()
partOne c = do
  let paths = thFirst c [] "start"
  putStrLn $ "Part 1 Paths = " ++ show paths
  return ()

partTwo :: [Connection] -> IO ()
partTwo c = do
  let paths = rjFirst c [] False "start"
  putStrLn $ "Part 2 Paths = " ++ show paths
  return ()


