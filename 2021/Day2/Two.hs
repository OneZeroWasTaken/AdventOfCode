module Two where

import           Data.Char

main :: IO ()
main = do
  content <- readFile "input.txt"
  let l = lines content
  partOne l
  putStrLn ""
  partTwo l
  return ()

partOne :: [String] -> IO ()
partOne d = do
  let n = navigate d (0, 0)
  putStrLn $ "Part one navigation: " ++ show n
  putStrLn $ "Answer: " ++ show (uncurry (*) n)
  return ()


partTwo :: [String] -> IO ()
partTwo d = do
  let (a, x, y) = navigateAim d (0, 0, 0)
  putStrLn $ "Part two navigation: " ++ show (a, x, y)
  putStrLn $ "Answer: " ++ show (x * y)
  return ()


navigateAim :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
navigateAim []       (a, x, y) = (a, x, y)
navigateAim (e : es) (a, x, y) = case head e of
  'f' ->
    navigateAim es (a, x + digitToInt (last e), y + digitToInt (last e) * a)
  'd' -> navigateAim es (a + digitToInt (last e), x, y)
  'u' -> navigateAim es (a - digitToInt (last e), x, y)
  _   -> error $ "Input " ++ e


navigate :: [String] -> (Int, Int) -> (Int, Int)
navigate []       (x, y) = (x, y)
navigate (e : es) (x, y) = case head e of
  'f' -> navigate es (x + digitToInt (last e), y)
  'd' -> navigate es (x, y + digitToInt (last e))
  'u' -> navigate es (x, y - digitToInt (last e))
  _   -> error $ "Input " ++ e


