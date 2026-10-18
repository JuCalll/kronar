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