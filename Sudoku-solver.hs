import System.IO
import Data.Char

type Puzzle = [[Int]]

main :: IO()
main = do
    file <- readFile "sudoku.txt"
    let output = solve (splitIntoPuzzles (lines file))
    writeFile "result.txt" output

splitIntoPuzzles :: [String] -> [[String]]
splitIntoPuzzles [] = []
splitIntoPuzzles file = 
    let (onePuzzle, rest) = splitAt 10 file
    in onePuzzle : splitIntoPuzzles rest

solve :: [[String]] -> String
solve [] = ""
solve (x : xs) = solve_h x ++ solve xs

solve_h :: [String] -> String
solve_h [] = ""
solve_h (puzzleNum : puzzle) = 
    (puzzleNum ++ ['\n']) ++ format (solveOnePuzzle (convertToPuzzle puzzle))

convertToPuzzle :: [String] -> Puzzle
convertToPuzzle = map (map digitToInt)

solveOnePuzzle :: Puzzle -> Puzzle
solveOnePuzzle [] = []
solveOnePuzzle (row : puzzle) = (map (*0) row) : solveOnePuzzle  puzzle

format :: Puzzle -> String
format rows = unlines (map formatRow rows)
      where formatRow row = unwords (map show row)
