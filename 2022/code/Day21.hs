module Day21 where

import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence                 as S

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input21.txt"
  let seq = S.fromList . map (filter (not . null) . splitOneOf ": ") $ l
  let root = S.index seq . fromJust . S.findIndexL ((== "root") . head) $ seq
  let job        = parseJob root seq
  let (Op l _ r) = job

  putStrLn $ "Part 1: root=" ++ (show . eval) job
  putStrLn $ "Part 2:    x=" ++ (show . eval . solveForX l) r

eval :: Job -> Int
eval (X   n    ) = n
eval (Lit n    ) = n
eval (Op j op k) = toOp op (eval j) (eval k)

solveForX :: Job -> Job -> Job
solveForX j n | containsX n = solveForX n j
solveForX (X _) n           = n
solveForX (Op j "+" k) n | containsX j = solveForX j (Op n "-" k)
                         | otherwise   = solveForX k (Op n "-" j)
solveForX (Op j "-" k) n | containsX j = solveForX j (Op n "+" k)
                         | otherwise   = solveForX k (Op j "-" n)
solveForX (Op j "*" k) n | containsX j = solveForX j (Op n "/" k)
                         | otherwise   = solveForX k (Op n "/" j)
solveForX (Op j "/" k) n | containsX j = solveForX j (Op n "*" k)
                         | otherwise   = solveForX k (Op j "/" n)

containsX :: Job -> Bool
containsX (X _     ) = True
containsX (Op j _ k) = containsX j || containsX k
containsX _          = False

data Job = Lit Int | Op Job String Job | X Int

parseJob :: [String] -> S.Seq [String] -> Job
parseJob ["humn", n] _ = X $ read n
parseJob [name, a, op, b] seq =
  Op (parseJob (findName a seq) seq) op (parseJob (findName b seq) seq)
parseJob [name, lit] seq = Lit (read lit)

findName :: String -> S.Seq [String] -> [String]
findName name seq =
  S.index seq . fromJust . S.findIndexL ((== name) . head) $ seq

toOp :: String -> (Int -> Int -> Int)
toOp s = case s of
  "+" -> (+)
  "-" -> (-)
  "*" -> (*)
  "/" -> div

