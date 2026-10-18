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