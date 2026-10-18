module Main where

import Kronar hiding (main)

-- Official boards, worked out by hand

-- t1 = [3, -2, -1, 4, 2]. The last square is 4 (not 0), so the goal is position 4.
--
--   | Path          | Values            | Raw | Sapphire | Ether | Final |
--   |---------------|-------------------|-----|----------|-------|-------|
--   | 0, 1, 2, 3, 4 | 3, -2, -1, 4, 2   |  6  |    -5    |  +10  |  11   |
--   | 0, 1, 2, 4    | 3, -2, -1, 2      |  2  |    -5    |  +10  |   7   |
--   | 0, 1, 3, 4    | 3, -2, 4, 2       |  7  |     0    |   0   |   7   |
--   | 0, 1, 4       | 3, -2, 2          |  3  |     0    |   0   |   3   |
--   | 0, 2, 3, 4    | 3, -1, 4, 2       |  8  |     0    |  +10  |  18   |
--   | 0, 2, 4       | 3, -1, 2          |  4  |     0    |  +10  |  14   |
--   | 0, 3, 4       | 3, 4, 2           |  9  |     0    |   0   |   9   |
--
--   Best: [0, 2, 3, 4] with 18 points. It jumps over the -2, so the -2 and
--   the -1 are never consecutive in the path and the Sapphire Rule is avoided.

-- t2 = [1, -3, -4, 2, 0]. The last square is 0, so the Void Rule moves the
-- goal to position 3. Position 4 is out of play.
--
--   | Path       | Values         | Raw | Sapphire | Ether | Final |
--   |------------|----------------|-----|----------|-------|-------|
--   | 0, 1, 2, 3 | 1, -3, -4, 2   | -4  |    -5    |  +10  |   1   |
--   | 0, 1, 3    | 1, -3, 2       |  0  |     0    |  +10  |  10   |
--   | 0, 2, 3    | 1, -4, 2       | -1  |     0    |   0   |  -1   |
--   | 0, 3       | 1, 2           |  3  |     0    |   0   |   3   |
--
--   Best: [0, 1, 3] with 10 points. A raw score of 0 is even, so it earns the bonus.

-- t3 = [5, 5, 5, 5, 5, 5]. The goal is position 5 and there are 13 paths.
-- Every value is 5, so a path with k squares has a raw score of 5k, no
-- Sapphire penalty, and the Ether bonus only when 5k is even (k even).
--
--   | Squares in the path (k) | Paths | Raw | Ether | Final |
--   |-------------------------|-------|-----|-------|-------|
--   | 6                       |   1   | 30  |  +10  |  40   |
--   | 5                       |   4   | 25  |   0   |  25   |
--   | 4                       |   6   | 20  |  +10  |  30   |
--   | 3                       |   2   | 15  |   0   |  15   |
--
--   Best: [0, 1, 2, 3, 4, 5] with 40 points, the only path that visits every square.

-- Custom boards, worked out by hand

-- Custom board 1: Void Rule + Ether Rule + tie broken by fewer jumps.
--
--   Board [4, 1, 0, 3, 0, 0]. The last two squares are 0, so the Void Rule
--   moves the goal back to position 3. Positions 4 and 5 are out of play.
--   It also shows that Dravek may land on a 0 in the middle of the path.
--
--   | Path       | Values     | Raw | Sapphire | Ether | Final |
--   |------------|------------|-----|----------|-------|-------|
--   | 0, 1, 2, 3 | 4, 1, 0, 3 |  8  |    0     |  +10  |  18   |
--   | 0, 1, 3    | 4, 1, 3    |  8  |    0     |  +10  |  18   |
--   | 0, 2, 3    | 4, 0, 3    |  7  |    0     |   0   |   7   |
--   | 0, 3       | 4, 3       |  7  |    0     |   0   |   7   |
--
--   Two paths tie at 18. [0, 1, 3] has 2 jumps and [0, 1, 2, 3] has 3,
--   so the first tie-breaker picks [0, 1, 3].
customBoard1 :: Board
customBoard1 = [4, 1, 0, 3, 0, 0]

-- Custom board 2: Sapphire Rule that cannot be avoided.
--
--   Board [-2, -3, -4, -1, 6]. The goal is position 4. Square 1 is negative
--   and every square Dravek can reach from it is negative too, so every path
--   pays at least one Sapphire penalty. Three negatives in a row are two pairs.
--
--   | Path          | Values             | Raw | Sapphire | Ether | Final |
--   |---------------|--------------------|-----|----------|-------|-------|
--   | 0, 1, 2, 3, 4 | -2, -3, -4, -1, 6  | -4  |   -15    |  +10  |  -9   |
--   | 0, 1, 2, 4    | -2, -3, -4, 6      | -3  |   -10    |   0   | -13   |
--   | 0, 1, 3, 4    | -2, -3, -1, 6      |  0  |   -10    |  +10  |   0   |
--   | 0, 1, 4       | -2, -3, 6          |  1  |    -5    |   0   |  -4   |
--   | 0, 2, 3, 4    | -2, -4, -1, 6      | -1  |   -10    |   0   | -11   |
--   | 0, 2, 4       | -2, -4, 6          |  0  |    -5    |  +10  |   5   |
--   | 0, 3, 4       | -2, -1, 6          |  3  |    -5    |   0   |  -2   |
--
--   The best path is [0, 2, 4] with 5 points. Note that -4 and 0 are even,
--   so they also earn the Ether bonus.
customBoard2 :: Board
customBoard2 = [-2, -3, -4, -1, 6]

-- Custom board 3: tie broken by dictionary order.
--
--   Board [1, -2, -2, 1, 3]. The goal is position 4.
--
--   | Path          | Values           | Raw | Sapphire | Ether | Final |
--   |---------------|------------------|-----|----------|-------|-------|
--   | 0, 1, 2, 3, 4 | 1, -2, -2, 1, 3  |  1  |    -5    |   0   |  -4   |
--   | 0, 1, 2, 4    | 1, -2, -2, 3     |  0  |    -5    |  +10  |   5   |
--   | 0, 1, 3, 4    | 1, -2, 1, 3      |  3  |     0    |   0   |   3   |
--   | 0, 1, 4       | 1, -2, 3         |  2  |     0    |  +10  |  12   |
--   | 0, 2, 3, 4    | 1, -2, 1, 3      |  3  |     0    |   0   |   3   |
--   | 0, 2, 4       | 1, -2, 3         |  2  |     0    |  +10  |  12   |
--   | 0, 3, 4       | 1, 1, 3          |  5  |     0    |   0   |   5   |
--
--   [0, 1, 4] and [0, 2, 4] tie at 12 with 2 jumps each. Compared square by
--   square, they differ at the second position: 1 < 2, so [0, 1, 4] wins.
customBoard3 :: Board
customBoard3 = [1, -2, -2, 1, 3]

-- Expected results

data TestCase = TestCase
  { caseName      :: String
  , caseBoard     :: Board
  , expectedPath  :: Path
  , expectedFinal :: Int
  , expectedCount :: Int
  }

testCases :: [TestCase]
testCases =
  [ TestCase "Statement example" [2, -1, -3, 5, 0]  [0, 1, 3]          16 4
  , TestCase "Official t1"       [3, -2, -1, 4, 2]  [0, 2, 3, 4]       18 7
  , TestCase "Official t2"       [1, -3, -4, 2, 0]  [0, 1, 3]          10 4
  , TestCase "Official t3"       [5, 5, 5, 5, 5, 5] [0, 1, 2, 3, 4, 5] 40 13
  , TestCase "Custom 1"          customBoard1       [0, 1, 3]          18 4
  , TestCase "Custom 2"          customBoard2       [0, 2, 4]           5 7
  , TestCase "Custom 3"          customBoard3       [0, 1, 4]          12 7
  ]

-- Running the tests

passes :: TestCase -> Bool
passes t =
  scoredPath s == expectedPath t
    && finalScore s == expectedFinal t
    && evaluatedPaths r == expectedCount t
  where
    r = solve (caseBoard t)
    s = best r

reportLine :: TestCase -> String
reportLine t
  | passes t  = "[PASS] " ++ caseName t
  | otherwise = "[FAIL] " ++ caseName t ++ ": expected " ++ show (expectedPath t)
                ++ " with " ++ show (expectedFinal t) ++ " points, got "
                ++ show (scoredPath s) ++ " with " ++ show (finalScore s)
  where
    s = best (solve (caseBoard t))

pathRow :: PathScore -> String
pathRow s = "  " ++ show (scoredPath s) ++ "  raw " ++ show (rawScore s)
            ++ ", sapphire -" ++ show (penalty s) ++ ", ether +" ++ show (bonus s)
            ++ ", final " ++ show (finalScore s)

pathTable :: Board -> String
pathTable b = unlines (("Board " ++ show b ++ ":") : map pathRow scores)
  where
    scores = map (scorePath b) (allPaths 0 (findGoal b))

invalidBoards :: [Board]
invalidBoards = [[7], [0, 0, 0], replicate 16 1]

validationLine :: Board -> String
validationLine b =
  case validateBoard b of
    Left err -> "[PASS] rejected " ++ show b ++ ": " ++ err
    Right _  -> "[FAIL] accepted " ++ show b

main :: IO ()
main = do
  putStrLn "=== Path tables for the custom boards ==="
  putStr (concatMap pathTable [customBoard1, customBoard2, customBoard3])
  putStrLn "=== Optimal path tests ==="
  mapM_ (putStrLn . reportLine) testCases
  putStrLn "=== Board validation tests ==="
  mapM_ (putStrLn . validationLine) invalidBoards
  putStrLn (show (length (filter passes testCases)) ++ " of "
            ++ show (length testCases) ++ " optimal path tests passed.")