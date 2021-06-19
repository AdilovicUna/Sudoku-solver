import System.IO
import Data.Char

type Puzzle = [[Int]]

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


