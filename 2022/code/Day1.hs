module Day1 where

import           Data.List

main :: IO ()
main = do
  content <- readFile "inputs/input1.txt"
  let l  = lines content
  let il = parseToIntLists l []
  let mc = maxCalories il
  putStrLn $ "Max calories: " ++ show mc

  let top = topMax 3 $ map sum il
  putStrLn $ "Top 3 calories: " ++ show (sum top)
  return ()

topMax :: Int -> [Int] -> [Int]
topMax 0 _  = []
topMax n il = maxim : topMax (n - 1) (delete maxim il)
  where maxim = maximum il

maxCalories :: [[Int]] -> Int
maxCalories = maximum . map sum

parseToIntLists :: [String] -> [Int] -> [[Int]]
parseToIntLists [] acc = [acc]
parseToIntLists (s : ss) acc | s == ""   = acc : parseToIntLists ss []
                             | otherwise = parseToIntLists ss (c : acc)
  where c = (read :: String -> Int) s
