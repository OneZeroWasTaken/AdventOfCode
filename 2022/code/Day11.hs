module Day11 where

import           Data.List
import           Data.Ord

main :: IO ()
main = do
  let insps1 = map inspections (round' (`div` 3) 20 monkeys)
  let insps2 = map inspections (round' (`mod` lcd) 10000 monkeys)
  putStrLn $ "Part 1 monkey business: " ++ (show . product . highest2) insps1
  putStrLn $ "Part 2 monkey business: " ++ (show . product . highest2) insps2
  return ()

highest2 :: [Int] -> [Int]
highest2 = take 2 . sortOn Down

round' :: (Integer -> Integer) -> Int -> [Monkey] -> [Monkey]
round' _ 0 ms = ms
round' f n ms = round' f (n - 1) $ foldl (inspect f) ms [0 .. length ms - 1]

inspect :: (Integer -> Integer) -> [Monkey] -> Int -> [Monkey]
inspect f ms i = changeItems c (replaceAt i newMonkey ms)
 where
  m         = ms !! i
  w2        = map (f . operation m) (worryLevel m)
  to        = map (test m) w2
  c         = zip w2 to
  newMonkey = throwAllItems $ addInspections m (length c)
  throwAllItems :: Monkey -> Monkey
  throwAllItems m' = m' { worryLevel = [] }
  addInspections :: Monkey -> Int -> Monkey
  addInspections m' n = m' { inspections = inspections m' + n }

changeItems :: [(Integer, Int)] -> [Monkey] -> [Monkey]
changeItems its ms = foldl changeItem ms its
 where
  changeItem :: [Monkey] -> (Integer, Int) -> [Monkey]
  changeItem ms (item, to) = replaceAt to toAfter ms
   where
    mTo     = ms !! to
    toAfter = mTo { worryLevel = worryLevel mTo ++ [item] }

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

test' :: Integer -> Int -> Int -> (Integer -> Int)
test' d true false n = if n `mod` d == 0 then true else false

data Monkey = Monkey
    { worryLevel  :: [Integer]
    , operation   :: Integer -> Integer
    , test        :: Integer -> Int
    , inspections :: Int
    }

instance Show Monkey where
  show m = show $ worryLevel m

lcd :: Integer
lcd = 3 * 11 * 19 * 5 * 2 * 7 * 17 * 13

monkeys :: [Monkey]
monkeys =
  [ defaultMonkey { worryLevel = [76, 88, 96, 97, 58, 61, 67]
                  , operation  = (* 19)
                  , test       = test' 3 2 3
                  }
  , defaultMonkey { worryLevel = [93, 71, 79, 83, 69, 70, 94, 98]
                  , operation  = (+ 8)
                  , test       = test' 11 5 6
                  }
  , defaultMonkey { worryLevel = [50, 74, 67, 92, 61, 76]
                  , operation  = (* 13)
                  , test       = test' 19 3 1
                  }
  , defaultMonkey { worryLevel = [76, 92]
                  , operation  = (+ 6)
                  , test       = test' 5 1 6
                  }
  , defaultMonkey { worryLevel = [74, 94, 55, 87, 62]
                  , operation  = (+ 5)
                  , test       = test' 2 2 0
                  }
  , defaultMonkey { worryLevel = [59, 62, 53, 62]
                  , operation  = \n -> n * n
                  , test       = test' 7 4 7
                  }
  , defaultMonkey { worryLevel = [62], operation = (+ 2), test = test' 17 5 7 }
  , defaultMonkey { worryLevel = [85, 54, 53]
                  , operation  = (+ 3)
                  , test       = test' 13 4 0
                  }
  ]

defaultMonkey :: Monkey
defaultMonkey = Monkey { worryLevel  = undefined
                       , operation   = undefined
                       , test        = undefined
                       , inspections = 0
                       }
