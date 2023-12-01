module Day01 where

import           Data.List
import           Data.Char

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input01.txt"

  putStrLn $ "Part 1: " ++ (show . sum . map endDigits) l
  putStrLn $ "Part 2: " ++ (show . sum . map (endDigits . spelledOutToDigits)) l

endDigits :: String -> Int
endDigits l = read [head ds, last ds] where ds = filter isDigit l

spelledOutToDigits :: String -> String
spelledOutToDigits l = map (\i -> f i (nums $ l !! i)) (take (length l) [0 ..])
  where f i = snd . head . filter (\(s, _) -> s `isPrefixOf` drop i l)

nums :: Char -> [(String, Char)]
nums c = zip spelledOut (map intToDigit [1 ..]) ++ [("", c)]

spelledOut :: [String]
spelledOut =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
