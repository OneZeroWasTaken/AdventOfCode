module Day19 where

import           Data.List
import           Data.List.Split
import           Data.Maybe

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/input19.txt"

  let
    test1 =
      parse
        "Blueprint 1: Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian."
  let
    test2 =
      parse
        "Blueprint 2: Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian."
  let bps = map parse l

  let s = maximum . filter (/= 0) . map geodes . iter test1 $ defaultState
  print s

  return ()

parse :: String -> Blueprint
parse s = Blueprint (head p)
                    (p !! 1)
                    (p !! 2)
                    (p !! 3, p !! 4)
                    (p !! 5, p !! 6)
 where
  p = map read $ filter (not . null) $ splitOneOf
    "Blueprint: Each ore robot costs ore. Each clay robot costs ore. Each obsidian robot costs ore and clay. Each geode robot costs ore and obsidian."
    s

iter :: Blueprint -> State -> [State]
iter b s | minutes s == 0 = [s]
         | otherwise      = concatMap (iter b . collect) actions
 where
  tryOre  = if oreCondition b s then Just $ oreBot b s else Nothing
  tryClay = if clayCondition b s then Just $ clayBot b s else Nothing
  tryObsidian =
    if obsidianCondition b s then Just $ obsidianBot b s else Nothing
  tryGeode = if geodeCondition b s then Just $ geodeBot b s else Nothing
  tryNone  = case (tryOre, tryClay) of
    (Just _ , _     ) -> s { previous = NoneCouldOre }
    (Nothing, Just _) -> s { previous = NoneCouldClay }
    _                 -> s { previous = None }
  actions = tryNone : catMaybes [tryOre, tryClay, tryObsidian, tryGeode]


oreCondition :: Blueprint -> State -> Bool
oreCondition b s | geodeCondition b s         = False
                 | previous s == NoneCouldOre = False
                 | oreBots s >= 3             = False
                 | ore s >= oreCost b         = True
                 | otherwise                  = False

clayCondition :: Blueprint -> State -> Bool
clayCondition b s | geodeCondition b s          = False
                  | previous s == NoneCouldClay = False
                  | clayBots s >= 5             = False
                  | ore s >= clayCost b         = True
                  | otherwise                   = False

obsidianCondition :: Blueprint -> State -> Bool
obsidianCondition b s
  | geodeCondition b s = False
  | ore s >= fst (obsidianCost b) && clay s >= snd (obsidianCost b) = True
  | otherwise          = False

geodeCondition :: Blueprint -> State -> Bool
geodeCondition b s
  | ore s >= fst (geodeCost b) && obsidian s >= snd (geodeCost b) = True
  | otherwise = False


oreBot :: Blueprint -> State -> State
oreBot b s =
  s { ore = ore s - oreCost b, oreBots = oreBots s + 1, previous = Ore }

clayBot :: Blueprint -> State -> State
clayBot b s =
  s { ore = ore s - clayCost b, clayBots = clayBots s + 1, previous = Clay }

obsidianBot :: Blueprint -> State -> State
obsidianBot b s = s { ore          = ore s - fst (obsidianCost b)
                    , clay         = clay s - snd (obsidianCost b)
                    , obsidianBots = obsidianBots s + 1
                    , previous     = Obsidian
                    }

geodeBot :: Blueprint -> State -> State
geodeBot b s = s { ore       = ore s - fst (geodeCost b)
                 , obsidian  = obsidian s - snd (geodeCost b)
                 , geodeBots = geodeBots s + 1
                 , previous  = Geode
                 }

collect :: State -> State
collect s = s { ore      = ore s + oreBots s
              , clay     = clay s + clayBots s
              , obsidian = obsidian s + obsidianBots s
              , geodes   = geodes s + geodeBots s
              , minutes  = minutes s - 1
              }

data State = State
    { ore :: Int
    , oreBots :: Int
    , clay :: Int
    , clayBots :: Int
    , obsidian :: Int
    , obsidianBots :: Int
    , geodes :: Int
    , geodeBots :: Int
    , minutes :: Int
    , previous :: Action
    } deriving Show

data Action = Ore | Clay | Obsidian | Geode | NoneCouldOre | NoneCouldClay | None
  deriving (Show, Eq)

defaultState :: State
defaultState = State { ore          = 0
                     , oreBots      = 1
                     , clay         = 0
                     , clayBots     = 0
                     , obsidian     = 0
                     , obsidianBots = 0
                     , geodes       = 0
                     , geodeBots    = 0
                     , minutes      = 22
                     , previous     = None
                     }

data Blueprint = Blueprint
    { nr :: Int
    , oreCost :: Int             -- Ore
    , clayCost :: Int            -- Ore
    , obsidianCost :: (Int, Int) -- (Ore, Clay)
    , geodeCost :: (Int, Int)    -- (Ore, Obsidian)
    } deriving (Show, Eq)


