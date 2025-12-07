module Day07 where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input07.txt"

  putStrLn $ "Part 1: " ++ show (beam1 ss [(start ss)])
  putStrLn $ "Part 2: " ++ show (beam2 ss [(start ss, 1)])

beam2 :: [String] -> [((Int, Int), Int)] -> Int
beam2 _ [] = 0
beam2 ss beams = outCount + beam2 ss mergedBeams
  where
    down' = map (\(p, n) -> (down p, n)) beams
    inside = filter (inBounds ss . fst) down'
    outCount = sum $ map snd $ filter (not . inBounds ss . fst) down'
    mergedBeams = merge . sort . concatMap (splitOnSplitter ss) $ inside

splitOnSplitter :: [String] -> ((Int, Int), Int) -> [((Int, Int), Int)] 
splitOnSplitter ss e@((x, y), n)
  | ss @ (x, y) == '^' = [((x - 1, y), n), ((x + 1, y), n)]
  | otherwise = return e

merge :: [((Int, Int), Int)] -> [((Int, Int), Int)]
merge [] = []
merge [e] = [e]
merge (a:b:rs) | fst a == fst b = merge ((fst a, snd a + snd b) : rs)
               | otherwise = a : merge (b:rs)

inBounds :: [String] -> (Int, Int) -> Bool
inBounds ss p = isJust (ss @? p)

beam1 :: [String] -> [(Int, Int)] -> Int
beam1 ss beams | null beams = 0
               | otherwise = splits + beam1 ss idk
  where
    d = map down beams
    idk = nub $ concatMap (\p -> case ss @? p of
        Just '^' -> split p
        Nothing -> []
        _   -> [p]
      ) d
    splits = length $ filter (\p -> case ss @? p of
        Just '^' -> True
        _        -> False
      ) d

(@?) :: [String] -> (Int, Int) -> Maybe Char
(@?) ss (x, y) = (ss !? y) >>= (\s -> s !? x)

(@) :: [String] -> (Int, Int) -> Char
(@) ss p = fromJust $ ss @? p

split :: (Int, Int) -> [(Int, Int)] 
split (x, y) = [(x - 1, y), (x + 1, y)]

down :: (Int, Int) -> (Int, Int)
down (x, y) = (x, y + 1)

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(e:_) !? 0 = Just e
(_:es) !? n | n > 0 = es !? (n - 1)
_ !? _ = Nothing

start :: [String] -> (Int, Int)
start ss = (fromJust (elemIndex 'S' (head ss)), 0)
