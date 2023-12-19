module Day19 where

import           Data.List.Split
import           Data.Char
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input19.txt"

  let (wfs, xmas) = parse l ([], [])
  let accepted'   = mapMaybe (accepted wfs (findWf "in" wfs)) xmas
  putStrLn $ "Part 1: " ++ show (sum $ map sumXmas accepted')
  putStrLn $ "Part 2: " ++ show ()

accepted :: [Workflow] -> Workflow -> Xmas -> Maybe Xmas
accepted wfs currentWf xmas = case appliedWf of
  Left  next -> accepted wfs (findWf next wfs) xmas
  Right b    -> if b then Just xmas else Nothing
  where appliedWf = f currentWf xmas

findWf :: String -> [Workflow] -> Workflow
findWf s = head . filter ((== s) . label)

data Workflow = Workflow
  { label :: String
  , f :: Xmas -> Either String Bool
  }

data Xmas = Xmas
  { x :: Int
  , m :: Int
  , a :: Int
  , s :: Int
  } deriving Show

sumXmas :: Xmas -> Int
sumXmas (Xmas x m a s) = x + m + a + s

parseWorkflow :: String -> Workflow
parseWorkflow s = Workflow label (wf fs)
 where
  (label : rules) = splitOneOf "{}," s
  fs              = map parseRule $ init rules
  wf (r : rs) xmas | appliedRule == Left "" = wf rs xmas
                   | otherwise              = appliedRule
    where appliedRule = r xmas

parseRule :: String -> (Xmas -> Either String Bool)
parseRule "A"                    = const $ Right True
parseRule "R"                    = const $ Right False
parseRule lbl | all isLetter lbl = const $ Left lbl
parseRule (xmasf : cmp : rs)     = \xmas ->
  if nf xmas `cmpf` read ns then parseRule out xmas else Left ""
 where
  cmpf      = if cmp == '<' then (<) else (>)
  nf        = xmasF xmasf
  [ns, out] = splitOn ":" rs

xmasF :: Char -> (Xmas -> Int)
xmasF 'x' = x
xmasF 'm' = m
xmasF 'a' = a
xmasF 's' = s

parse :: [String] -> ([Workflow], [Xmas]) -> ([Workflow], [Xmas])
parse [] acc = acc
parse (l : ls) (ss, xs) | null l        = parse ls (ss, xs)
                        | head l == '{' = parse ls (ss, Xmas x' m' a' s' : xs)
                        | otherwise     = parse ls (parseWorkflow l : ss, xs)
 where
  [x', m', a', s'] = map read . filter (not . null) . splitOneOf "{xmas}=," $ l
