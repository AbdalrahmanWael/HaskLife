module Main where
import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.Process (system)
import Control.Concurrent (threadDelay)
import System.Console.Terminal.Size (size, height, width, Window)

main :: IO ()
main = do
  terminalSize <- getTerminalSize
  let (rows, cols) = maybe (30, 60) id terminalSize
  initialGrid <- generateRandomGrid rows cols
  hideCursor
  runGenerations initialGrid 50000
  showCursor

data Cell = Alive | Dead
  deriving (Eq)

instance Show Cell where 
  show Alive = "█"
  show Dead = "░"

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

printGrid:: Grid -> IO ()
printGrid grid = mapM_ (putStrLn . concatMap show) grid

clearScreen:: IO () 
clearScreen = putStr "\ESC[H"  -- Moves the cursor to the top-left without clearing the screen

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l" 

showCursor :: IO ()
showCursor = putStr "\ESC[?25h" 

getTerminalSize:: IO (Maybe (Int, Int))
getTerminalSize = do
  maybeWindow <- size
  return $ fmap (\win -> (height win, width win)) maybeWindow

adjustGridToTerminal:: Grid -> IO Grid
adjustGridToTerminal grid = do 
  maybeSize <- getTerminalSize
  case maybeSize of 
    Just (h, w) -> return $ take (h - 2) $ map (take (w - 1)) grid
    Nothing -> return grid

runGenerations :: Grid -> Int -> IO ()
runGenerations grid 0 = return ()
runGenerations grid n = do
  clearScreen
  adjustedGrid <- adjustGridToTerminal grid
  let newGrid = evolveGrid adjustedGrid
  putStrLn $ "Generation " ++ show (50000 - n + 1) ++ ":"
  printGrid adjustedGrid
  threadDelay 100000  -- microseconds (0.1 seconds)
  runGenerations newGrid (n - 1)
