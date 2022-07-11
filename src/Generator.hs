module Generator where
import Data.List
import Data.Maybe
import System.Random
import Utils
import Solver

------------------------------- Generates a cloudy blank board -------------------------------

cloudBoard :: Int -> Int -> Int -> Board
cloudBoard n m minNum=  
    Board {cells = [[
        if 
            (((x>(n-3))||(y>(m-3)))&&((x>y+3)||(y>x+3)))||
            (((x>(n-2))||(y>(m-2)))&&((x>y+2)||(y>x+2)))||
            (((x>(n-1))||(y>(m-1)))&&((x>y+1)||(y>x+1))) 
        then (-1) 
        else (0) 
        | x <- [1..n]] | y <- [1..m]], 
        minNum = minNum, maxNum = n*m}

------------------------------- Generates a square blank board -------------------------------

squareBoard :: Int -> Int -> Int -> Board
squareBoard n m minNum=  
    Board {cells = [[0 | _ <- [1..n]] | _ <- [1..m]], minNum = minNum, maxNum = n*m}

------------------------------- Generates a mirror blank board -------------------------------

mirrorBoard :: Int -> Int -> Int -> Board
mirrorBoard n m minNum=  
    Board {cells = [[ if x==y then (-1) else (0)| x <- [1..n]] | y <- [1..m]], minNum = minNum, maxNum = n*m}

------------------------------- Main function to fill a board -------------------------------
fill :: Board -> (Int,Int) -> Board
fill board pos = newBoard
    where
        num = minNum board
        -- get last num for board based on empty spaces and min value
        lastNum = calculateZeros (cells board) + (minNum board) - 1 
        -- put initial element on board
        initialBoard = Board (replaceAt pos num (cells board)) (minNum board) lastNum
        -- fill board with solver
        newBoard = solveR initialBoard num pos 


------------------------------- function to find cells in the board between min and max -------------------------------

findPairs :: [[Int]] -> Int-> Int-> [(Int,Int)]
findPairs matrix max min = 
    [(x,y) | (x, rows) <-enumerate matrix, (y,value)<- enumerate rows, value < max, value >= min ]        
    where
        enumerate = zip [0..]

------------------------------- Make 0 some of the cells in the board. Recursive function -------------------------------

removeNum :: Board -> [(Int,(Int,Int))] -> Int -> Board
removeNum board randList n
    | n == 0 = board
    | otherwise = removeNum newBoard (rest) (n-1)
    where
        (init:rest) = (randList)
        (index,pos) = init
        newCells = replaceAt pos 0 (cells board)
        newBoard = Board newCells (minNum board) (maxNum board)

-- -------------------generate a list of random Ints
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)


------------ Generate a board ----------------
-- generate :: Int -> Int -> Utils.Board
-- a: width
-- b: height
-- minNum: minimum number 
-- shapeIndex: index of valids shapes [0: cloud, 1: square, 2: mirror]
generate a b minNum shapeIndex = do
    -------make the seed
    seed1 <- newStdGen

    ---------build the board with the specified form
    --------- mirrorBoard, cloudBoard and squareBoard
    ----------you pass the dimensions of the board and the min value
    let shapes = [(cloudBoard a b minNum),(mirrorBoard a b minNum),(squareBoard a b minNum)]
    let sel = shapes!!shapeIndex
    
    let blankBoard = (sel)
    
    -----------the initial position for first element is generated (index start at 0)
    let blankList = findPairs (cells blankBoard) (1) (0)
    randIndex <- randomRIO (0,(length (blankList)-1))
    let initIndex = blankList!!(randIndex)
    
    -------fill the board with numbers
    let board = fill blankBoard initIndex
    
    -------get a list for the ocupated positions excluding initial
    let listPos = findPairs (cells board) (maxNum board) (minNum + 1)
    let n = length (listPos)

    -----------make a list of random indexes to match with listPos and shuffle it
    let list = map(\x -> (mod x n)) (randomList n seed1)
    let eliminationList = sort (zip (list) (listPos))

    -----------replace with 0 the number of cells passed
    let total = maxNum board - minNum 
    let genBoard = removeNum board eliminationList (round ( fromIntegral total * 0.75 ))

    ------return
    return (genBoard)

