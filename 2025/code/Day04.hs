module Day04 where

main :: IO ()
main = do
  ss <- lines <$> readFile "inputs/input04.txt"

  let allPos = coords ss
      paperPos = filter (isPaper ss) allPos

  putStrLn $ "Part 1: " ++ show (length $ filter (accessable paperPos) allPos)
  putStrLn $ "Part 2: " ++ show (length paperPos - notAccessable paperPos)

notAccessable :: [(Int, Int)] -> Int
notAccessable ps = length . fst . head . dropWhile (uncurry (/=)) $ zip (scanPapers ps) (tail $ scanPapers ps)

scanPapers :: [(Int, Int)] -> [[(Int, Int)]]
scanPapers ps = scanl (\b _ -> filter (not . accessable b) b) ps [0 ..]

accessable :: [(Int, Int)] -> (Int, Int) -> Bool
accessable papers p = (p `elem` papers) && length (filter (`elem` papers) (neighbors p)) < 4

isPaper :: [String] -> (Int, Int) -> Bool
isPaper ss (x, y) = ss !! y !! x == '@'

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (sx, sy) = [(sx + x, sy + y) | x <- [-1 .. 1], y <- [-1 .. 1], not $ x == 0 && y == 0]

coords :: [String] -> [(Int, Int)]
coords ss = [(x, y) | x <- [0 .. length (head ss) - 1], y <- [0 .. length ss - 1]]
