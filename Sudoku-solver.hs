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
solve_h (puzzleNum : puzzle) = 
    let 
        puzzle' = convertToPuzzle puzzle
        storage = store puzzle'
    in
        (puzzleNum ++ ['\n']) ++ format (solveOnePuzzle puzzle' storage)

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
    | rows == Nothing = "No solution possible \n"
    | otherwise = unlines (map formatRow (fromJust rows))
    where formatRow row = unwords (map show row)

{-
    Function that checkes if the puzzle is solved
    (if there are any 0 values it will return false)
-}
allNonZero :: Puzzle -> Bool
allNonZero puzzle = all (/= 0) (concat puzzle)

{- 
    Main function for solving an individual Puzzle
-}  
solveOnePuzzle :: Puzzle -> Storage -> Maybe Puzzle
solveOnePuzzle puzzle storage
    | allNonZero puzzle = Just puzzle
    | otherwise = 
        let 
            var = (mrv storage)
            loop :: [Int] -> Pos -> Puzzle -> Storage -> Maybe Puzzle
            loop [] _ puzzle _ = Just puzzle
            loop (x : xs) pos puzzle storage = 
                let 
                    newPuzzle = fillValue puzzle (x,pos)
                    newStorage = updateStorage storage (x,pos)
                in solveOnePuzzle newPuzzle newStorage
        in 
            if var == Nothing then Nothing 
            else 
                let 
                    (num, possible_vals, (i,j)) = fromJust var
                in
                    loop possible_vals (i,j) puzzle storage    

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
store puzzle = [store_h puzzle (i, row) | (i, row) <- [0 ..] `zip` puzzle]

{-
    Function that stores all possible values for every
    variable in one row of the puzzle
-}
store_h :: Puzzle -> (Int,[Int]) -> [Square]    
store_h puzzle (i,row) =
    [ eVals
        | (vals, j) <- row `zip` [0..],
        let eVals = if vals /= 0 then Filled vals else Empty (getValues (i, j) puzzle (transpose puzzle)) ]

{-
    Function that goes through each row of the puzzle 
    and fills the new value with the help of fillValue_h
-}
fillValue :: Puzzle -> (Int, Pos) -> Puzzle
fillValue puzzle v = [fillValue_h puzzle (i, row) v | (i, row) <- [0 ..] `zip` puzzle]

{-
    Function that copies all previous values to the new puzzle
    when it finds the value on the position (i,j)
    it excahnges it previous value with a new one
-}
fillValue_h :: Puzzle -> (Int,[Int]) -> (Int, Pos) -> [Int] 
fillValue_h puzzle (i',row) (val,(i,j)) = [if i' == i && j' == j then val else val'
                                |  (val', j') <- row `zip` [0..]]

{-
    Function that updates storage when one value is filled
    with the help of updateStorage_h
-}
updateStorage :: Storage -> (Int, Pos) -> Storage
updateStorage storage v = [updateStorage_h storage (i, row) v | (i, row) <- [0 ..] `zip` storage]

{-
    Function that updates one row of values in storage
-}
updateStorage_h :: Storage -> (Int,[Square]) -> (Int, Pos) -> [Square] 
updateStorage_h storage (i', row) (val,(i,j)) = 
    [if i' == i && j' == j then Filled val else val' |   (temp, j') <- row `zip` [0..], 
        let 
            val' =  if checkIfFilled temp 
                        then temp 
                    else if i == i' || j == j' || (i `div` 3, j `div` 3) == (i' `div` 3, j' `div` 3)
                        then remove val temp 
                    else temp]

{-
    Function that returnes True if Square is Filled
-}
checkIfFilled :: Square -> Bool
checkIfFilled (Filled _) = True
checkIfFilled (Empty _) = False

{-
    Function that removes one element from an Empty Square
-}
remove :: Int -> Square -> Square
remove x (Empty xs) = Empty (delete x xs)

{-
    Function that returns all posible values for a specific position in the Puzzle
-}
getValues ::Pos -> Puzzle -> Puzzle -> [Int]
getValues (i,j) wholeGrid wholeGridT= 
    let 
        row = wholeGrid !! i
        column = wholeGridT !! j
        field = getField (i,j) wholeGrid
    in   
        (\\) ( (\\) ( (\\) [1..9]  row) column) field

{-
    Function that gets a a block in the Puzzle based on the position passed
-}
getField :: Pos -> Puzzle -> [Int]
getField (i, j) wholeGrid =
    [val | (row, i') <- wholeGrid `zip` [0..], (val, j') <- row `zip` [0..],
           i' `div` 3 == i `div` 3 && j' `div` 3 == j `div` 3]