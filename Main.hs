import Rules
import Control.Concurrent

filename = "board.txt"

toStatus :: Int -> CellState
toStatus n
  |n == 0 = Off
  |otherwise = On


readBoard :: IO Board
readBoard = do
  contents <- readFile filename
  let rawBoard = map words (lines contents)
  let inInts =  ((map . map) read rawBoard)
  let inStatuses = (map . map) toStatus inInts
  return (inStatuses, width, height) where
    width = 8
    height = 8

toString :: CellState -> String
toString On = "+ "
toString Off = "_ "

putBoard :: Board -> IO()
putBoard board = putStrLn (concat (map (++ "\n") (map show (stateStrings)))) where
  stateStrings = map concat ((map . map) toString (states board))

main = do
  let iterations = 100
  board <- readBoard
  putBoard board
  mainHelper iterations board

mainHelper 0 board = putStrLn "\n"
mainHelper iterations board = do
  let nextBoard = tick board
  putBoard nextBoard
  threadDelay(500000)
  mainHelper (iterations -1) nextBoard
  
  
