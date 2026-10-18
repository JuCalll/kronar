module Kronar where

-- Input

inputBoard :: Board
inputBoard = [3, -2, -1, 4, 2]

-- Types

type Board = [Int]

type Path = [Int]

data PathScore = PathScore
  { scoredPath :: Path
  , rawScore   :: Int
  , penalty    :: Int
  , bonus      :: Int
  , finalScore :: Int
  } deriving (Show, Eq)

data GameResult = GameResult
  { board          :: Board
  , goal           :: Int
  , voidApplied    :: Bool
  , best           :: PathScore
  , evaluatedPaths :: Int
  } deriving (Show, Eq)

-- Board validation

minSquares :: Int
minSquares = 2

maxSquares :: Int
maxSquares = 15

validateBoard :: Board -> Either String Board
validateBoard b
  | length b < minSquares = Left ("The board must have at least " ++ show minSquares
                                  ++ " squares, but it has " ++ show (length b) ++ ".")
  | length b > maxSquares = Left ("The board can have at most " ++ show maxSquares
                                  ++ " squares, but it has " ++ show (length b) ++ ".")
  | all (== 0) b          = Left "The board must have at least one square with a value other than 0."
  | otherwise             = Right b

-- Void Rule

findGoal :: Board -> Int
findGoal b = last [i | i <- [0 .. length b - 1], b !! i /= 0]

voidRuleApplied :: Board -> Int -> Bool
voidRuleApplied b g = g /= length b - 1

-- Path generation

allPaths :: Int -> Int -> [Path]
allPaths current target
  | current == target = [[target]]
  | otherwise         = [ current : rest
                        | next <- [current + 1 .. min target (current + 3)]
                        , rest <- allPaths next target ]

-- Rules and score

pathValues :: Board -> Path -> [Int]
pathValues b p = map (b !!) p

rawScoreOf :: [Int] -> Int
rawScoreOf values = sum values

sapphirePenalty :: [Int] -> Int
sapphirePenalty (a : b : rest)
  | a < 0 && b < 0 = 5 + sapphirePenalty (b : rest)
  | otherwise      = sapphirePenalty (b : rest)
sapphirePenalty _  = 0

etherBonus :: Int -> Int
etherBonus raw
  | even raw  = 10
  | otherwise = 0

scorePath :: Board -> Path -> PathScore
scorePath b p = PathScore
  { scoredPath = p
  , rawScore   = raw
  , penalty    = pen
  , bonus      = bon
  , finalScore = raw - pen + bon
  }
  where
    values = pathValues b p
    raw    = rawScoreOf values
    pen    = sapphirePenalty values
    bon    = etherBonus raw