{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day24 where

import           Data.List.Split
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input24.txt"

  putStrLn $ "Part 1: " ++ show (binary $ map eval $ parse l z)
  putStrLn $ "Part 2: " ++ show ()

z :: [String]
z =
  reverse
    $  [ "z0" ++ show n | n <- [0 .. 9] ]
    ++ [ "z" ++ show n | n <- [10 .. 45] ]

binary :: [Bool] -> Int
binary =
  foldr (\n acc -> n + 2 * acc) 0 . reverse . map (\b -> if b then 1 else 0)

data Expr = Expr Expr (Bool -> Bool -> Bool) Expr | Const Bool

eval :: Expr -> Bool
eval (Expr e1 op e2) = eval e1 `op` eval e2
eval (Const b      ) = b

type ExprMap = Map String (Either (String, Bool -> Bool -> Bool, String) Bool)

parse :: [String] -> [String] -> [Expr]
parse ss = map createExpr
 where
  createExpr :: String -> Expr
  createExpr s = case fromJust $ M.lookup s m of
    Left  (e1, op, e2) -> Expr (createExpr e1) op (createExpr e2)
    Right b            -> Const b
  m = foldl f M.empty ss
  f :: ExprMap -> String -> ExprMap
  f m' s | '-' `elem` s = M.insert sn (Left (g1, toOp op, g2)) m'
         | ':' `elem` s = M.insert (take 3 s) (Right $ toBool $ last s) m'
         | otherwise    = m'
   where
    [g1, op, g2, _, sn] = splitOn " " s
    toOp :: String -> (Bool -> Bool -> Bool)
    toOp "AND" = (&&)
    toOp "OR"  = (||)
    toOp "XOR" = (/=)
    toBool :: Char -> Bool
    toBool '0' = False
    toBool '1' = True
