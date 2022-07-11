module Solver where
import Data.List
import Data.Maybe
import Debug.Trace
import Utils

--------------------------- Finds the position of an element  ---------------------------
findElement :: [[Int]] -> Int -> (Int, Int)
findElement cells el =  (x, y)
    where
        x = findLine cells el 0
        y = fromJust $ elemIndex el (cells!!x)
        
------------------------- Returns the row of an element in the list -------------------------
findLine :: [[Int]] -> Int -> Int -> Int 
findLine cells el pos
    | pos == length cells = error "Element not found"
    | el `elem` (cells !! pos) = pos
    | otherwise = findLine cells el (pos + 1)


------- Check if number is placed in board ------
numInBoard :: Int -> Board -> Bool
numInBoard num board = any (\x -> x == num) (concat cellList)
    where
        cellList = cells board

----------- Check if number is neighbor of given position in the board ------------
isNeighbor :: Int -> Board -> (Int, Int) -> Bool
isNeighbor num board (x, y) =  any (\x -> x == num ) 
            [cellList!!(x+x1)!!(y+y1) | x1 <- dx, y1 <- dy, inRange (x+x1, y+y1) cellList] 
    where
        cellList = cells board
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]
        

------------------------------- Main function to solve a board -------------------------------
solve :: Board -> IO ()
solve board =
    printBoard newBoard
    where
        num = minNum board
        -- Se empieza a llenar el tablero por donde esta el menor elemento
        pos = findElement (cells board) num
        -- se llama a la funcion recursiva para resolver el tablero
        newBoard = solveR board num pos 


----------------------------- Recursive function to solve the board -------------------------------
solveR :: Board -> Int -> (Int, Int)  -> Board
-- num: last num positioned in board
-- (x, y): coordinates of last positioned num
solveR board num (x, y) 
    -- | num > maxNum board = Empty
    -- If num is max in board finish
    | num == maxNum board = board
    -- If next num in current's neighbors repet process for next
    | isNeighbor (num + 1) board (x, y) = solveR board (num + 1) (findElement (cells board) (num +1)) 
    -- If next num is in board and is not a neighbor then board is invalid
    | numInBoard (num + 1) board = Empty
    -- If not empty cell in neighbors board is invalid
    | cantContinue (x, y) (dx, dy) (cells board) =  Empty
    -- Otherwise try to position 
    | otherwise = maybeBoard
    where
        -- Directions 
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]
        -- Recursive search in neighbors
        maybeBoard = recursiveCall 0
        recursiveCall index
            -- All neighbors were visited
            | index == length dx = Empty
            -- If is not a valid neighbor then try next
            | not (inRange (newX, newY) cellList) || cellList!!newX!!newY /= 0 = recursiveCall (index + 1)
            -- If a valid solution is found returns it
            | solution /= Empty = solution
            -- Otherwise recursive call for next neighbor
            | otherwise = recursiveCall (index + 1)
            where
                cellList = cells board
                -- New position
                (newX, newY) = (x + dx!!index, y+dy!!index)
                -- Generate new board
                newCells = replaceAt (newX, newY) (num + 1) cellList
                newBoard = Board newCells (minNum board) (maxNum board)
                -- Search in new board
                solution = solveR newBoard (num + 1) (newX, newY) 


