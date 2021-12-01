module One where


main :: IO ()
main = do
  content <- readFile "input.txt"
  let l = lines content
  let d = map (read :: String -> Int) l
  partOne d
  partTwo d
  return ()

partOne :: [Int] -> IO ()
partOne d = do
  let count = countIncreases d
  putStrLn $ "Part one count: " ++ show count
  return ()

partTwo :: [Int] -> IO ()
partTwo d = do
  let count = threeCountIncreases d
  putStrLn $ "Part two count: " ++ show count
  return ()


threeCountIncreases :: [Int] -> Int
threeCountIncreases (a : b : c : d : rest)
  | a + b + c < b + c + d = 1 + threeCountIncreases (b : c : d : rest)
  | otherwise             = threeCountIncreases (b : c : d : rest)
threeCountIncreases _ = 0

countIncreases :: [Int] -> Int
countIncreases []       = 0
countIncreases (i : is) = countIncreases' i is
 where
  countIncreases' _ [] = 0
  countIncreases' p (e : es) | e > p     = 1 + countIncreases' e es
                             | otherwise = countIncreases' e es


