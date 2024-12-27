{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day21 where

main :: IO ()
main = do
  --ss <- lines <$> readFile "inputs/input21.txt"

  let a = part1 "^A<<A^^>>AvvvA"   -- 319A
      b = part1 "^^A<<^A>vvvA>A"   -- 670A
      c = part1 "^A<<^A^>>AvvvA"   -- 349A
      d = part1 "^^^AvA<<A>>vvA"   -- 964A
      e = part1 "<^^A^Av>AvvA"     -- 586A
  putStrLn $ "Part 1: " ++ show
    (319 * a + 670 * b + 349 * c + 964 * d + 586 * e)

part2 :: String -> Int
part2 = length . (!! 25) . iterate (keypad 'A')

part1 :: String -> Int
part1 = length . keypad 'A' . keypad 'A'

keypad :: Char -> String -> String
keypad _   []         = []
keypad 'A' ('v' : ss) = "v<A" ++ keypad 'v' ss
keypad 'A' ('<' : ss) = "v<<A" ++ keypad '<' ss
keypad 'A' ('^' : ss) = "<A" ++ keypad '^' ss
keypad 'A' ('>' : ss) = "vA" ++ keypad '>' ss
keypad 'A' ('A' : ss) = "A" ++ keypad 'A' ss
keypad '^' ('v' : ss) = "vA" ++ keypad 'v' ss
keypad '^' ('<' : ss) = "v<A" ++ keypad '<' ss
keypad '^' ('^' : ss) = "A" ++ keypad '^' ss
keypad '^' ('>' : ss) = "v>A" ++ keypad '>' ss
keypad '^' ('A' : ss) = ">A" ++ keypad 'A' ss
keypad '<' ('v' : ss) = ">A" ++ keypad 'v' ss
keypad '<' ('<' : ss) = "A" ++ keypad '<' ss
keypad '<' ('^' : ss) = ">^A" ++ keypad '^' ss
keypad '<' ('>' : ss) = ">>A" ++ keypad '>' ss
keypad '<' ('A' : ss) = ">>^A" ++ keypad 'A' ss
keypad 'v' ('v' : ss) = "A" ++ keypad 'v' ss
keypad 'v' ('<' : ss) = "<A" ++ keypad '<' ss
keypad 'v' ('^' : ss) = "^A" ++ keypad '^' ss
keypad 'v' ('>' : ss) = ">A" ++ keypad '>' ss
keypad 'v' ('A' : ss) = ">^A" ++ keypad 'A' ss
keypad '>' ('v' : ss) = "<A" ++ keypad 'v' ss
keypad '>' ('<' : ss) = "<<A" ++ keypad '<' ss
keypad '>' ('^' : ss) = "<^A" ++ keypad '^' ss
keypad '>' ('>' : ss) = "A" ++ keypad '>' ss
keypad '>' ('A' : ss) = "^A" ++ keypad 'A' ss

