import System.IO
import Data.Char
import Data.List


example = ["003020600", "900305001", "001806400", "008102900", "700000008", "006708200", "002609500", "800203009", "005010300"]

type Puzzle = [[Int]]
type Pos = (Int,Int)

main :: IO()
main = do
    file <- readFile "sudoku.txt"
    let output = solve (splitIntoPuzzles (lines file))
    writeFile "result.txt" output

{- 
    Function that splits the file into individual sudoku grids
-}
splitIntoPuzzles :: [String] -> [[String]]
splitIntoPuzzles [] = []
splitIntoPuzzles file = 
    let (onePuzzle, rest) = splitAt 10 file
    in onePuzzle : splitIntoPuzzles rest

{-
    Function that builds the string of solutions one by one
    with the help of solve_h
-}
solve :: [[String]] -> String
solve [] = ""
solve (x : xs) = solve_h x ++ solve xs

{-
    Function that formats a solved sudoku grid
    To do that it prepends the number of the grid,
    and the formatted solution of the Puzzle
-}
solve_h :: [String] -> String
solve_h (puzzleNum : puzzle) = (puzzleNum ++ ['\n']) ++ format (solveOnePuzzle (convertToPuzzle puzzle))

{-
    Function that simply converts sudoku grid from string format into a Puzzle
-}
convertToPuzzle :: [String] -> Puzzle
convertToPuzzle = map (map digitToInt)

{-
    Converts a Puzzle back to sudoku grid in string representation
-}
format :: Puzzle -> String
format rows = unlines (map formatRow rows)
      where formatRow row = unwords (map show row)

{- 
    main function for solving an individual Puzzle
-}
solveOnePuzzle :: Puzzle -> Puzzle
solveOnePuzzle [] = []
solveOnePuzzle (row : puzzle) = (map (*0) row) : solveOnePuzzle  puzzle

{-
    Function for minimum value heuristic
    It will return list of possible values for the place in the puzzle
    with smallest amount of possible values and position of that place
-}
mrv :: Puzzle -> Puzzle -> Pos -> ([Int],Pos) -> ([Int],Pos)
mrv [] _ _ prev = prev
mrv (x : xs) wholeGrid curr prev = 
    let 
        newPrev = one_row_mrv x wholeGrid curr prev
        newCurr = (fst curr + 1, 0)
    in 
        mrv xs wholeGrid newCurr newPrev

{-
    Function that find the minimum value heuristic for one row
-}
one_row_mrv :: [Int] -> Puzzle -> Pos -> ([Int],Pos) -> ([Int],Pos)
one_row_mrv [] _ _ prev = prev
one_row_mrv (x : xs) wholeGrid curr (possiblePrev, (xPrev,yPrev)) = 
    let 
        next = (fst curr, snd curr + 1)
        possibleCurr = getValues curr wholeGrid (transpose wholeGrid)
    in
        if x /= 0 || length possiblePrev <= length possibleCurr 
            then one_row_mrv xs wholeGrid next (possiblePrev, (xPrev,yPrev))
        else one_row_mrv xs wholeGrid next (possibleCurr, curr)

{-
    Function that returns all posible values for a specific position in the Puzzle
-}
getValues ::Pos -> Puzzle -> Puzzle -> [Int]
getValues (x,y) wholeGrid wholeGridT= 
    let 
        row = wholeGrid !! x
        column = wholeGridT !! y
        field = getField (x,y) wholeGrid
    in  
        do 
           removeValues (removeValues (removeValues [1..9] row) column) field
        where 
            removeValues :: [Int] -> [Int] -> [Int]
            removeValues xs ys = [x | x <- xs, not (elem x ys)]

{-
    Function that gets a a block in the Puzzle based on the position passed
-}
getField :: Pos -> Puzzle -> [Int]
getField (x,y) wholeGrid = 
    let 
        rowX = wholeGrid !! x
        rowXPlus1 = wholeGrid !! (x+1)
        rowXMinus1 = wholeGrid !! (x-1)

        slicedRows = case mod x 3 of
            0 -> [rowX, rowXPlus1, wholeGrid !! (x+2)]
            1 -> [rowXMinus1, rowX, rowXPlus1]
            2 -> [wholeGrid !! (x-2), rowXMinus1, rowX]

        slicedRowsT = transpose slicedRows

        columnY = slicedRowsT !! y
        columnYPlus1 = slicedRowsT !! (y+1)
        columnYMinus1 = slicedRowsT !! (y-1)

        slicedColumns = case mod y 3 of
            0 -> [columnY, columnYPlus1, slicedRowsT !! (y+2)]
            1 -> [columnYMinus1, columnY, columnYPlus1]
            2 -> [slicedRowsT !! (y-2), columnYMinus1, columnY]
    in concat slicedColumns