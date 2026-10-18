module Kronar where
import Data.List (intercalate)
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

-- Optimal path and tie-breakers

rankKey :: PathScore -> (Int, Int, Path)
rankKey s = (negate (finalScore s), length (scoredPath s), scoredPath s)

bestPath :: [PathScore] -> PathScore
bestPath scores = foldr1 better scores
  where
    better x y
      | rankKey x <= rankKey y = x
      | otherwise              = y

solve :: Board -> GameResult
solve b = GameResult
  { board          = b
  , goal           = g
  , voidApplied    = voidRuleApplied b g
  , best           = bestPath scores
  , evaluatedPaths = length scores
  }
  where
    g      = findGoal b
    scores = map (scorePath b) (allPaths 0 g)

-- JSON

jsonBool :: Bool -> String
jsonBool True  = "true"
jsonBool False = "false"

jsonIntList :: [Int] -> String
jsonIntList xs = show xs

jsonField :: String -> String -> String
jsonField key value = "  \"" ++ key ++ "\": " ++ value

toJson :: GameResult -> String
toJson r = "{\n" ++ intercalate ",\n" fields ++ "\n}\n"
  where
    s = best r
    fields =
      [ jsonField "tablero"              (jsonIntList (board r))
      , jsonField "casilla_meta"         (show (goal r))
      , jsonField "regla_vacio_aplicada" (jsonBool (voidApplied r))
      , jsonField "camino_optimo"        (jsonIntList (scoredPath s))
      , jsonField "puntaje_bruto"        (show (rawScore s))
      , jsonField "penalizacion_zafiro"  (show (penalty s))
      , jsonField "bono_eter"            (show (bonus s))
      , jsonField "puntaje_final"        (show (finalScore s))
      , jsonField "caminos_evaluados"    (show (evaluatedPaths r))
      ]

-- Console summary

toSquareNumbers :: Path -> [Int]
toSquareNumbers p = map (+ 1) p

summary :: GameResult -> String
summary r = unlines
  [ "=== The Game of the Kronars ==="
  , "Board:           " ++ show (board r)
  , "Goal:            square " ++ show (goal r + 1) ++ voidNote
  , "Optimal path:    squares " ++ show (toSquareNumbers (scoredPath s))
  , "Raw score:       " ++ show (rawScore s)
  , "Sapphire Rule:   -" ++ show (penalty s)
  , "Ether Rule:      +" ++ show (bonus s)
  , "Final score:     " ++ show (finalScore s)
  , "Paths evaluated: " ++ show (evaluatedPaths r)
  ]
  where
    s = best r
    voidNote
      | voidApplied r = " (Void Rule applied)"
      | otherwise     = ""

-- Main

outputFile :: FilePath
outputFile = "resultado.json"

main :: IO ()
main =
  case validateBoard inputBoard of
    Left err -> putStrLn ("Invalid board: " ++ err ++ "\nNo file was created.")
    Right b  -> do
      let result = solve b
      putStr (summary result)
      writeFile outputFile (toJson result)
      putStrLn ("File " ++ outputFile ++ " created.")
