module Rules
       (
         Board
        ,Cell
        ,CellState(On, Off)
        ,tick
        ,states
       )
       where

type Cell = (Int, Int)

type Board = ([[CellState]], Int, Int)
dim1 (_, x, _) = x
dim2 (_, _, y) = y

allCells :: Board -> [Cell]
allCells board = [(x, y) | x <- [0 .. (dim1 board) - 1], y <- [0 .. (dim2 board) - 1]]

getCellStatus :: Board -> Cell -> CellState
getCellStatus board (x, y) = ((states board) !! x) !! y

states:: Board -> [[CellState]]
states (s, _, _) = s

data CellState = On | Off deriving (Eq, Show)

nextState :: Board -> Cell -> CellState
nextState board cell
  |state == On &&  n < 2 = Off
  |state == On &&  n <= 3 = On
  |state == On &&  n > 3 = Off
  |state == Off && n == 3 = On
  |otherwise = Off
  where
    state = getCellStatus board cell
    n = numLiveNeighbors board cell

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

tick :: Board -> Board
tick board =  (splitEvery (dim1 board) (map (nextState board) (allCells board)), dim1 board, dim2 board)

numLiveNeighbors :: Board -> Cell -> Int
numLiveNeighbors board cell = length (filter (== On) neighbors) where
  neighbors = map (getCellStatus board)
    [((x - 1) `mod` x1, (y - 1) `mod` y1),
     ((x - 1) `mod` x1, y `mod` y1),
     ((x - 1) `mod` x1, (y + 1) `mod` y1),
     (x `mod` x1, (y - 1) `mod` y1),
     (x `mod` x1, (y + 1) `mod` y1),
     ((x + 1) `mod` x1, (y - 1) `mod` y1),
     ((x + 1) `mod` x1, y `mod` y1),
     ((x + 1) `mod` x1, (y + 1) `mod` y1)] where
      x1 = dim1 board
      y1 = dim2 board
      x = fst cell
      y = snd cell

