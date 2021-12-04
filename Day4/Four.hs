module Four where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe

main :: IO ()
main = do
  content <- readFile "input.txt"
  let l      = lines content
  let input  = extractInput $ head l
  let bingos = extractBingos $ tail l
  partOne bingos input
  putStrLn ""
  partTwo bingos input
  return ()

partOne :: [Bingo] -> [Int] -> IO ()
partOne bingos input = do
  let (winBingo, i) = simWin bingos input
  let sumU          = sumUnmarked winBingo
  putStrLn $ "Winning Bingo: " ++ show (winBingo, i)
  putStrLn $ "Sum of unmarked: " ++ show sumU
  putStrLn $ "Part 1 score = " ++ show (sumU * i)
  return ()

partTwo :: [Bingo] -> [Int] -> IO ()
partTwo bingos input = do
  let (loseBingo, i) = simLose bingos input
  let sumU           = sumUnmarked loseBingo
  putStrLn $ "Loosing Bingo: " ++ show (loseBingo, i)
  putStrLn $ "Sum of unmarked: " ++ show sumU
  putStrLn $ "Part 2 score = " ++ show (sumU * i)
  return ()


newtype Bingo = Bingo [[Cell]]
    deriving (Show, Eq)

data Cell = Unmarked Int | Marked Int
    deriving (Show, Eq)


extractInput :: String -> [Int]
extractInput i = map read (splitOn "," i)

extractBingos :: [String] -> [Bingo]
extractBingos []        = []
extractBingos ("" : es) = extractBingos es
extractBingos es        = createBingo (take 5 es) : extractBingos (drop 5 es)

createBingo :: [String] -> Bingo
createBingo b = Bingo $ map (cellRow . splitOn " ") b
 where
  cellRow :: [String] -> [Cell]
  cellRow []        = []
  cellRow ("" : es) = cellRow es
  cellRow (i  : es) = Unmarked (read i :: Int) : cellRow es


simLose :: [Bingo] -> [Int] -> (Bingo, Int)
simLose [b] (i : is)
  | isNothing $ checkWin $ markAll [b] i = simLose (markAll [b] i) is
  | otherwise                            = (head $ markAll [b] i, i)
simLose bs (i : is) = simLose (removeWins $ markAll bs i) is
simLose _  []       = error "No lose"

removeWins :: [Bingo] -> [Bingo]
removeWins [] = []
removeWins (Bingo b : bs) | checkOne b || checkOne (transpose b) = removeWins bs
                          | otherwise = Bingo b : removeWins bs


simWin :: [Bingo] -> [Int] -> (Bingo, Int)
simWin _ [] = error "No win"
simWin bs (i : is) | isNothing win = simWin allMarked is
                   | otherwise     = (fromJust win, i)
 where
  win       = checkWin allMarked
  allMarked = markAll bs i


markAll :: [Bingo] -> Int -> [Bingo]
markAll bs i = map (`mark` i) bs

mark :: Bingo -> Int -> Bingo
mark (Bingo cs) x = Bingo
  (map
    (map
      (\c -> case c of
        Unmarked y | x == y -> Marked x
        z                   -> z
      )
    )
    cs
  )

checkWin :: [Bingo] -> Maybe Bingo
checkWin [] = Nothing
checkWin (Bingo cs : bs) | checkOne cs             = Just (Bingo cs)
                         | checkOne (transpose cs) = Just (Bingo cs)
                         | otherwise               = checkWin bs

checkOne :: [[Cell]] -> Bool
checkOne [] = False
checkOne (r : rs) | checkRow r = True
                  | otherwise  = checkOne rs
 where
  checkRow []                  = True
  checkRow ((Unmarked _) : _ ) = False
  checkRow ((Marked   _) : cs) = checkRow cs

sumUnmarked :: Bingo -> Int
sumUnmarked (Bingo []      ) = 0
sumUnmarked (Bingo (r : rs)) = sumUnmarked (Bingo rs) + sumRow r
 where
  sumRow []                = 0
  sumRow (Unmarked c : cs) = sumRow cs + c
  sumRow (_          : cs) = sumRow cs

