module Utils where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split
import Data.Maybe


-- generate at least 2 types of board in blank
-- defining a standard way (hearted or rombus or cloud)
------------------------------- Board Definition -------------------------------
data Board = Board {
    cells :: [[Int]],
    minNum :: Int,
    maxNum :: Int
} | Empty deriving (Show, Eq)

------------------------ Returns if position is in range in a matrix  ------------------------
inRange :: (Int, Int) -> [[Int]] -> Bool
inRange (x, y) matrix =
    x >= 0 && x < nRow && y >= 0 && y < nCol
    where
        nRow = length matrix
        nCol = length (matrix !! x) 


------------------------------- Prints the board -------------------------------
printBoard :: Board -> IO ()
printBoard Empty = putStrLn "Empty"
printBoard board = putStrLn (showBoard board)

------------------------------- Returns the board in a string -------------------------------
showBoard :: Board -> String
showBoard board = showCells cellList 0 0 (length cellList) (length line) maxSpace
    where
        cellList = cells board
        line = head cellList
        maxSpace = length (show (maxNum board))


-------------------------- Returns the cells in a String --------------------------
showCells :: [[Int]] -> Int -> Int-> Int-> Int -> Int -> String
showCells cellList pRow pCol endRow endCol maxSpace
    | pRow == endRow = ""
    | pCol == endCol = "\n" ++ showCells cellList (pRow + 1) 0 endRow (length line) maxSpace
    | otherwise = spaces ++ elem ++ showCells cellList pRow (pCol + 1) endRow endCol maxSpace
        where
            line = cellList !! (pRow + 1)
            el =  cellList !! pRow !! pCol
            elem = if el == -1 then " " else show el
            spaces =  concat [" " | _ <- [0..(maxSpace - length elem)]]


------------------------------- Replace an element in a matrix  -------------------------------
-- Recibe la posicion, el numero y la matriz y retorna la matriz con el elemento reemplazado
replaceAt :: Eq a => (Int, Int) -> a -> [[a]] -> [[a]]
replaceAt (x,y) num cellList = a1 ++ (newLine:b1)
    where
        -- encuentra la linea dividiendo por las filas
        (a1, line:b1) = splitAt x cellList
        -- encuentra la posicon dividiendo la linea en la columna
        (a2, _:b2) = splitAt y line
        newLine = a2 ++ (num:b2)


----------------------- Calculates the amount of zeros in the matrix -----------------------
calculateZeros :: [[Int]] -> Int
calculateZeros matrix = sum ([ 1 | x <- concat matrix, x==0])

----------------------- Decides if there is no num to put in the board -----------------------
-- Si todas las celdas alrededor son distintas de 0 entonces estan ocupadas y no se puede continuar
cantContinue :: (Int, Int) -> ([Int], [Int]) -> [[Int]] -> Bool
cantContinue (x, y) (dx, dy) cellList =
        0 `notElem` [cellList!!(x+nX)!!(y+nY) | nX <- dx, nY <- dy, inRange (x+nX,y+nY) cellList]

serializeBoard :: Board -> String
serializeBoard Empty = ""
serializeBoard board = serializeCells cellList 0 0 rowLenght
    where
        cellList = cells board
        rowLenght = maximum [(length row) | row <- cellList]

-------------------------- Serialize cells in a String --------------------------
serializeCells :: Show a => [[a]] -> Int -> Int -> Int -> [Char]
serializeCells cellList pRow pCol endRow 
    | pRow == length cellList = ""
    | pCol == endRow - 1 = elem ++ "\n" ++ serializeCells cellList (pRow + 1) 0 endRow 
    | otherwise = elem ++ " " ++ serializeCells cellList pRow (pCol + 1) endRow  
        where
            line = cellList !! (pRow + 1)
            el =  cellList !! pRow !! pCol
            elem = show el

writeBoard :: Board -> String -> IO ()
writeBoard board file = 
    do
        let str = serializeBoard board
        writeFile file str


parseBoard :: String -> Board
parseBoard str = 
    Board cellList minNum maxNum
    where
        rows = lines str
        cellList = [ [ read x :: Int | x <- splitOn " " line] | line <- rows]
        minNum = minimum [x | x <- concat cellList , x /= -1 && x /= 0] 
        maxNum = maximum (concat cellList)

loadBoard :: String -> IO Board
loadBoard file = 
    do
        str <- readFile file
        -- print ("Loading board from " ++ file ++ " with:\n" ++ str)
        return (parseBoard str)