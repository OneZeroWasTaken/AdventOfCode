module Ten where

import           Data.Char
import           Data.List.Split
import           Data.List
import           Data.Maybe
import           Data.Ord

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  partOne input
  putStrLn ""
  partTwo input
  return ()

partTwo :: [String] -> IO ()
partTwo s = do
  let sorted = sort $ completionScore s
  print sorted
  putStrLn $ "Completion score = " ++ show (middle sorted)
  return ()

middle :: [a] -> a
middle a = a !! (length a `div` 2)

partOne :: [String] -> IO ()
partOne s = do
  putStrLn $ "Syntax error score = " ++ show (syntaxScore s)
  return ()

data Output = Corrupted Char | Incomplete String | Complete
    deriving (Show, Eq)

completionScore :: [String] -> [Int]
completionScore []       = []
completionScore (i : is) = case parse i "" of
  Corrupted  _ -> completionScore is
  Incomplete s -> incompleteScore s : completionScore is
  Complete     -> completionScore is

incompleteScore :: String -> Int
incompleteScore = foldl (\s' c -> s' * 5 + score2 c) 0

syntaxScore :: [String] -> Int
syntaxScore []       = 0
syntaxScore (i : is) = case parse i "" of
  Corrupted  c -> syntaxScore is + score c
  Incomplete _ -> syntaxScore is
  Complete     -> syntaxScore is

parse :: String -> String -> Output
parse []       []    = Complete
parse []       stack = Incomplete stack
parse (i : is) []    = parse is [i]
parse (i : is) (s : stack) | isClosing i && isMatching s i = parse is stack
                           | isClosing i = Corrupted i
                           | otherwise = parse is (i : s : stack)

isMatching :: Char -> Char -> Bool
isMatching '(' ')' = True
isMatching '[' ']' = True
isMatching '{' '}' = True
isMatching '<' '>' = True
isMatching _   _   = False

isClosing :: Char -> Bool
isClosing c = score c > 0

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _   = 0

score2 :: Char -> Int
score2 '(' = 1
score2 '[' = 2
score2 '{' = 3
score2 '<' = 4
score2 _   = 0
