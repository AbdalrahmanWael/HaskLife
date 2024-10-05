module Main where
import System.Random (randomRIO)
import Control.Monad (replicateM)

main :: IO ()
main = do
  initialGrid <- generateRandomGrid 30 60 
  putStrLn "Initial Grid:"
  printGrid initialGrid
  runGenerations initialGrid 50000

data Cell = Alive | Dead
  deriving (Eq)

instance Show Cell where 
  show Alive = "█"
  show Dead = " "

type Row = [Cell]
type Grid = [Row]

generateRandomGrid :: Int -> Int -> IO Grid
generateRandomGrid rows cols = do
  let generateRow = sequence [randomCell | _ <- [1..cols]]
  sequence [generateRow | _ <- [1..rows]]
  where
    randomCell = do
      r <- randomRIO (0, 1) :: IO Int
      return $ if r == 0 then Dead else Alive


initialGrid:: Grid 
initialGrid = [[Dead, Dead, Alive], [Alive, Alive, Alive], [Dead, Dead, Dead]]

evolveGrid:: Grid -> Grid 
evolveGrid grid = 
  [ [ evolveCell (grid !! y !! x) (getNeighbors grid x y) | x <- [0..length (head grid) - 1] ] | y <- [0..length grid - 1] ]


evolveCell:: Cell -> [Cell] -> Cell
evolveCell Alive neighbors
  | aliveCount < 2  = Dead
  | aliveCount > 3  = Dead
  | otherwise = Alive
  where aliveCount = countAlive neighbors
evolveCell Dead neighbors 
  | aliveCount == 3 = Alive 
  | otherwise       = Dead
  where aliveCount = countAlive neighbors

countAlive:: [Cell] -> Int
countAlive = length . filter (Alive ==)

-- 8 neighbors around (x, y), [x-1, x+2], [y-1, y+1] where x is column, y is row
getNeighbors :: Grid -> Int -> Int -> [Cell] 
getNeighbors grid x y = [grid !! j !! i | i <- [x-1..x+1], j <- [y-1..y+1], (i /= x || j /= y), isValid grid i j]


isValid:: Grid -> Int -> Int -> Bool
isValid grid i j = i >= 0 && j >= 0 && j < length grid && i < length  (grid !! 0)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ (putStrLn . concatMap show) grid

-- printGrid :: Grid -> IO ()
-- printGrid [] = return () 
-- printGrid (row:rows) = do
--   putStrLn (concatMap show row) 
--   printGrid rows               

runGenerations :: Grid -> Int -> IO ()
runGenerations grid 0 = return ()
runGenerations grid n = do
  let newGrid = evolveGrid grid
  putStrLn $ "\nGeneration " ++ show (n) ++ ":"
  printGrid newGrid
  runGenerations newGrid (n - 1)
