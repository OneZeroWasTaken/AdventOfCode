module Day7 where

import           Data.Char
import           Data.List.Split

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input7.txt"
  let fs        = parseFiles (tail $ parseLines l) ["/"] root
      allSizes  = map size (allDirs fs)
      below100k = sum . filter (< 100000) $ allSizes
  putStrLn $ "Part 1 Sum of all directories below 100k: " ++ show below100k
  let spaceNeeded = updateSize - (diskSize - size fs)
      toDelete    = minimum . filter (>= spaceNeeded) $ allSizes
  putStrLn $ "Part 2 Delete directory with size:        " ++ show toDelete
  return ()


data File = File Int String | Directory String [File]
    deriving (Eq, Show)

data Line = CD String | LS | ListDirectory String | ListFile String
    deriving (Eq, Show)

size :: File -> Int
size (File      s _ ) = s
size (Directory _ fs) = sum (map size fs)

allDirs :: File -> [File]
allDirs (    File      _ _ ) = []
allDirs dir@(Directory _ fs) = dir : concatMap allDirs fs

parseLines :: [String] -> [Line]
parseLines [] = []
parseLines (l : ls)
  | take 4 l == "$ cd" = CD (drop 5 l) : parseLines ls
  | l == "$ ls"        = LS : parseLines ls
  | take 3 l == "dir"  = ListDirectory (drop 4 l) : parseLines ls
  | isDigit (head l)   = ListFile l : parseLines ls
  | otherwise          = error "Cannot parse"

parseFiles :: [Line] -> [String] -> File -> File
parseFiles []       _  f = f
parseFiles (l : ls) cd f = case l of
  CD d            -> parseFiles ls (changeDirectory d cd) f
  LS              -> parseFiles ls cd f
  ListDirectory s -> parseFiles ls cd $ addFile (Directory s []) cd f
  ListFile      s -> parseFiles ls cd (addFile file cd f)
   where
    [size, name] = splitOn " " s
    file         = File (read size) name

addFile :: File -> [String] -> File -> File
addFile _ _  file@(File      _ _ ) = file
addFile f [] (     Directory s fs) = Directory s (f : fs)
addFile f [path] (Directory s fs) | path == s = Directory s (f : fs)
                                  | otherwise = Directory s fs
addFile f path (Directory s fs) | dir == s  = Directory s $ map (addFile f p) fs
                                | otherwise = Directory s fs
 where
  dir = last path
  p   = init path

changeDirectory :: String -> [String] -> [String]
changeDirectory directory cd | d == ".." = tail cd
                             | otherwise = d : cd
  where d = (last . splitOn " ") directory

root :: File
root = Directory "/" []

diskSize :: Int
diskSize = 70000000

updateSize :: Int
updateSize = 30000000
