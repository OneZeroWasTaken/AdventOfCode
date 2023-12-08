module Day08 where

import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input08.txt"

  let (lrs, t) = parse l
      part1    = distToZZZ lrs t "AAA"
      part2    = foldl1 lcm $ map (zEvery lrs t 0) (findA t)
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

type Table = [(String, (String, String))]

zEvery :: String -> Table -> Integer -> String -> Integer
zEvery (lr : lrs) t n c | (== 'Z') . last $ c = n
                        | otherwise           = zEvery lrs t (n + 1) next'
  where next' = next t lr c

next :: Table -> Char -> String -> String
next t lr = chooseLR lr . fromJust . (`lookup` t)

chooseLR :: Char -> (String, String) -> String
chooseLR 'L' = fst
chooseLR 'R' = snd

findA :: Table -> [String]
findA = map fst . filter ((== 'A') . last . fst)

distToZZZ :: String -> Table -> String -> Int
distToZZZ _          _ "ZZZ" = 0
distToZZZ (lr : lrs) t c     = 1 + distToZZZ lrs t (next t lr c)

parse :: [String] -> (String, Table)
parse ss = (cycle $ head ss, t)
 where
  t = map (\[a, b, c] -> (a, (b, c))) l
  l = map (filter (not . null) . splitOneOf " =(),") . drop 2 $ ss
