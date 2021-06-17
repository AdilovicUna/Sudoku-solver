import System.IO

main :: IO()
main = do
    file <- readFile "sudoku.txt"
    writeFile  "result.txt" ""
    solve (splitIntoPuzzles (lines file))

splitIntoPuzzles :: [String] -> [[String]]
splitIntoPuzzles [] = []
splitIntoPuzzles file = 
    let s = splitAt 10 file
        onePuzzle = fst s
        rest = snd s
    in onePuzzle : splitIntoPuzzles rest

solve :: [[String]] -> IO()
solve [] = return()
solve ( (puzzleNum : puzzle) : rest) = do
    appendFile  "result.txt"  (puzzleNum ++ ['\n'])
    let result = solveOnePuzzle (convertToInt puzzle)
    appendFile  "result.txt" (format result)
    solve rest

convertToInt :: [String] -> [[Int]]
convertToInt [] = []
convertToInt (row : puzzle) = (map (read . (:"")) row :: [Int]) : convertToInt puzzle

solveOnePuzzle :: [[Int]] -> [[Int]]
solveOnePuzzle [] = []
solveOnePuzzle (row : puzzle) = (map (*0) row) : solveOnePuzzle  puzzle

format :: [[Int]] -> String
format [] = ""
format (row : puzzle) = unwords (map show row) ++ ['\n'] ++ format puzzle
