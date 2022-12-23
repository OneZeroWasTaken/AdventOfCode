module Day23 where

import           Data.List
import           Data.Maybe
import qualified Data.Set                      as S

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input23.txt"

  let set     = S.fromList $ parseElves l (Pos 0 0)
  let round10 = foldl round' set [0 .. 9]
  let reg     = region $ S.toList round10

  putStrLn $ "Part 1 area:   " ++ show (uncurry (*) reg - length set)
  putStrLn $ "Part 2 rounds: " ++ show (roundsUntilStopped set 0)

  return ()

roundsUntilStopped :: S.Set Pos -> Int -> Int
roundsUntilStopped set n | S.difference set next == S.empty = n + 1
                         | otherwise = roundsUntilStopped next (n + 1)
  where next = round' set n

region :: [Pos] -> (Int, Int)
region ps = (maximum xs - minimum xs + 1, maximum ys - minimum ys + 1)
 where
  xs = map gx ps
  ys = map gy ps

round' :: S.Set Pos -> Int -> S.Set Pos
round' set n = move set $ cleanPropositions allProps
 where
  allProps  = catMaybes . S.toList . S.map (propose thisOrder set) $ set
  thisOrder = (take 4 . drop n . cycle) order

order :: [[Pos -> Pos]]
order = [north, south, west, east]

move :: S.Set Pos -> [(Pos, Pos)] -> S.Set Pos
move set props = foldr S.insert (foldr S.delete set allFrom) allTo
 where
  allFrom = map fst props
  allTo   = map snd props

cleanPropositions :: [(Pos, Pos)] -> [(Pos, Pos)]
cleanPropositions props = filter (\(f, t) -> t `elem` allowedTo) props
 where
  allowedTo = concat . filter ((== 1) . length) . group . sort $ map snd props

propose :: [[Pos -> Pos]] -> S.Set Pos -> Pos -> Maybe (Pos, Pos)
propose fss set p
  | (not . hasAdjacent set) p = Nothing
  | otherwise = case f of
    Nothing -> Nothing
    Just f' -> Just (p, go p (head f'))
  where f = find (not . any ((`S.member` set) . go p)) fss

hasAdjacent :: S.Set Pos -> Pos -> Bool
hasAdjacent es p = any ((`S.member` es) . go p) allDir

parseElves :: [String] -> Pos -> [Pos]
parseElves [] _ = []
parseElves (s : ss) p | null s        = parseElves ss (stepD p { gx = 0 })
                      | head s == '#' = p : parseElves (tail s : ss) (stepR p)
                      | otherwise     = parseElves (tail s : ss) (stepR p)

go :: Pos -> (Pos -> Pos) -> Pos
go p f = f p

allDir :: [Pos -> Pos]
allDir =
  [ stepR
  , stepD
  , stepL
  , stepU
  , stepR . stepD
  , stepR . stepU
  , stepL . stepD
  , stepL . stepU
  ]

east, south, west, north :: [Pos -> Pos]
east = [stepR, stepD . stepR, stepU . stepR]
south = [stepD, stepR . stepD, stepL . stepD]
west = [stepL, stepD . stepL, stepU . stepL]
north = [stepU, stepR . stepU, stepL . stepU]

stepR, stepD, stepL, stepU :: Pos -> Pos
stepR q = q { gx = gx q + 1 }
stepD q = q { gy = gy q + 1 }
stepL q = q { gx = gx q - 1 }
stepU q = q { gy = gy q - 1 }

data Pos = Pos { gx :: Int, gy :: Int }
 deriving Eq

instance Ord Pos where
  Pos x1 y1 <= Pos x2 y2 = (x1, y1) <= (x2, y2)

instance Show Pos where
  show (Pos x y) = 'P' : show (x, y)

