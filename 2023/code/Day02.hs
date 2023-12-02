module Day02 where

import           Data.List
import           Data.List.Split
import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input02.txt"

  let games = map (maxCubes . parseGame) l
  putStrLn $ "Part 1: " ++ show ((sumIds . filterPossible) games)
  putStrLn $ "Part 2: " ++ show (sumOfPower games)

sumIds :: [Game] -> Int
sumIds = sum . map (\(Game i _ _ _) -> i)

sumOfPower :: [Game] -> Int
sumOfPower = sum . map (\(Game _ r g b) -> r * g * b)

filterPossible :: [Game] -> [Game]
filterPossible = filter (\(Game _ r g b) -> r <= 12 && g <= 13 && b <= 14)

parseGame :: String -> [String]
parseGame = splitOneOf ":;," . filter (/= ' ') . drop 5

maxCubes :: [String] -> Game
maxCubes (i : rounds) = foldr extractColor (Game (read i) 0 0 0) rounds

extractColor :: String -> (Game -> Game)
extractColor s
  | "red" `isSuffixOf` s   = \(Game i r g b) -> Game i (max n r) g b
  | "green" `isSuffixOf` s = \(Game i r g b) -> Game i r (max n g) b
  | "blue" `isSuffixOf` s  = \(Game i r g b) -> Game i r g (max n b)
  where n = read $ takeWhile isDigit s

data Game = Game Int Int Int Int
