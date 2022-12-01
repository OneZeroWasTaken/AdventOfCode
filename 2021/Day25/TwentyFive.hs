module TwentyFive where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

test :: [Cucumber]
test =
  parseCucumbers
    $ lines
        "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"
        --"...>...\n.......\n......>\nv.....>\n......>\n.......\n..vvv.."


main :: IO ()
main = do
  content <- readFile "input.txt"
  let cucumbers = test
  --let cucumbers = parseCucumbers $ lines content
  partOne cucumbers
  putStrLn ""
  --partTwo
  return ()

parseCucumbers :: [String] -> [Cucumber]
parseCucumbers []       = []
parseCucumbers (s : ss) = parseLine s ++ parseCucumbers ss
 where
  parseLine [] = []
  parseLine (c : cs)
    | c == 'v' = South (fst size - length cs, snd size - length ss) False
    : parseLine cs
    | c == '>' = East (fst size - length cs, snd size - length ss) False
    : parseLine cs
    | otherwise = parseLine cs


data Cucumber = East (Int, Int) Bool | South (Int, Int) Bool
 deriving Show

instance Eq Cucumber where
  c1 == c2 = pos c1 == pos c2

pos :: Cucumber -> (Int, Int)
pos (East  c _) = c
pos (South c _) = c

isEast :: Cucumber -> Bool
isEast (East _ _) = True
isEast _          = False

size :: (Int, Int)
--size = (99, 99)
size = (9, 9)
--size = (6, 6)

nextPos :: Cucumber -> Cucumber
nextPos (East (x, y) _) | x == fst size = East (0, y) False
                        | otherwise     = East (x + 1, y) False
nextPos (South (x, y) _) | y == snd size = South (x, 0) False
                         | otherwise     = South (x, y + 1) False

moveIfAvailable :: Cucumber -> [Cucumber] -> Cucumber
moveIfAvailable c cs | nextPos c `elem` cs = setMove c False
                     | otherwise           = setMove c True

moveSouth :: [Cucumber] -> [Cucumber] -> [Cucumber]
moveSouth [] done = done
moveSouth (c : cs) done
  | (not . isEast) c = moveSouth cs (moveIfAvailable c (cs ++ done) : done)
  | otherwise        = moveSouth cs (c : done)

moveEast :: [Cucumber] -> [Cucumber] -> [Cucumber]
moveEast [] done = done
moveEast (c : cs) done
  | isEast c  = moveEast cs (moveIfAvailable c (cs ++ done) : done)
  | otherwise = moveEast cs (c : done)

stepMoving :: [Cucumber] -> (Int, [Cucumber])
stepMoving = helper 0
 where
  helper n [] = (n, [])
  helper n (c : cs) | moving c  = (m + 1, nextPos c : next)
                    | otherwise = (m, c : next)
    where (m, next) = helper n cs

moving :: Cucumber -> Bool
moving (East  _ b) = b
moving (South _ b) = b

setMove :: Cucumber -> Bool -> Cucumber
setMove (East  p _) b = East p b
setMove (South p _) b = South p b

step :: [Cucumber] -> (Int, [Cucumber])
step cs = (n + m, south)
 where
  (n, east ) = stepMoving $ moveEast cs []
  (m, south) = stepMoving $ moveSouth east []

stepUntilStop :: [Cucumber] -> (Int, [Cucumber])
stepUntilStop = step' 1
 where
  step' n cs | s == 0    = (n, cs)
             | otherwise = step' (n + 1) next
    where (s, next) = step cs

partOne :: [Cucumber] -> IO ()
partOne cs = do
  let steps = stepUntilStop cs
  putStrLn $ "Part 1 Steps = " ++ show steps
  return ()

partTwo :: IO ()
partTwo = do
  putStrLn $ "Part 2 Cubes = " ++ show ()
  return ()



