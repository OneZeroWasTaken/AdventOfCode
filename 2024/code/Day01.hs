module Day01 where


main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input01.txt"
  putStrLn $ "Part 1: "
