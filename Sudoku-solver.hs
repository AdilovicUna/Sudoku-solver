import System.IO
import Data.Char
import Data.List
import Data.Maybe

example = ["003020600", "900305001", "001806400", "008102900", "700000008", "006708200", "002609500", "800203009", "005010300"]

type Puzzle = [[Int]]
type Pos = (Int,Int)
type Storage = [[Square]]
data Square = Filled Int | Empty [Int]
    deriving (Show)

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
solve xs = concatMap solve_h xs

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
format :: Maybe Puzzle -> String
format rows
    | rows == Nothing = "No solution possible"
    | otherwise = unlines (map formatRow (fromJust rows))
    where formatRow row = unwords (map show row)

{-
    Function that checkes if the puzzle is solved
    (if there are any 0 values it will return false)
-}
allNonZero :: Puzzle -> Bool
allNonZero [] = True
allNonZero (x : xs) =
    let 
        (a,b) = partition (==0) x
    in
        length b == 0 && allNonZero xs

{- 
    Main function for solving an individual Puzzle
-}  
solveOnePuzzle :: Puzzle -> Maybe Puzzle
solveOnePuzzle puzzle
    | allNonZero puzzle = Just puzzle
    | otherwise = Just puzzle

{-
    Function for minimum value heuristic
    It will return list of possible values for the place in the puzzle
    with smallest amount of possible values and position of that place
    Returns a triple (num, values, pos), where 'num' is the minimum number of
    possible values, 'values' are the actual values, and 'pos' is a position.
-}      
mrv :: Storage -> Maybe (Int, [Int], Pos)
mrv storage = 
    let 
        list = [(length vals, vals, (i, j)) | (row, i) <- storage `zip` [0 ..], (Empty vals, j) <- row `zip` [0 ..] ]
    in
        if length list == 0 then Nothing else Just (minimum list)

{- 
    Function that goes through each row of the puzzle 
    and stores it with the help of store_h
-}
store :: Puzzle -> Storage
store puzzle = [store_h puzzle i | i <- [0 .. length puzzle - 1]]

{-
    Function that stores all possible values for every
    variable in one row of the puzzle
-}
store_h :: Puzzle -> Int -> [Square]    
store_h puzzle i =
    [ eVals
        | j <- [0 .. length puzzle - 1],
        let vals = (puzzle !! i) !! j
            eVals = if vals /= 0 then Filled vals else Empty (getValues (i, j) puzzle (transpose puzzle)) ]

{-
    Function that updates storage when one value is filled
-}
updateStorage :: Storage -> (Int, Pos) -> Storage
updateStorage storage (val,(i,j)) = storage
    

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
        (\\) ( (\\) ( (\\) [1..9]  row) column) field

{-
    Function that gets a a block in the Puzzle based on the position passed
-}
getField :: Pos -> Puzzle -> [Int]
getField (x, y) wholeGrid =
    [val | (row, i) <- wholeGrid `zip` [0..], (val, j) <- row `zip` [0..],
           i `div` 3 == x `div` 3 && j `div` 3 == y `div` 3]