#!/usr/bin/env runhaskell
-- I used Data.Array library to solve this problem.
import Data.Array
-- Below code solves the example.
main = do
    let solution = solved pBoard
    printBoard solution
-- The Number(function) on the board are represented by Ints in the range 0,1,2,3,4 numbers. However 0 number represents "empty cell".
-- "type" is declared data.
type Number = Int
type L = (Int, Int) 
type B = Array L Number
-- The sudoku board needs to be solved.
-- This is 4x4 matrix but array is starting from 0 so in line 16, the array will be from 0 to 3.
pBoard :: B
pBoard = array ((0, 0), (3, 3)) $ pAssocs ePuzzle
-- Sudoku example is given in the question. 
-- In bracket Number, the data will be called from above lines 10,11,12.  
ePuzzle :: [[Number]]
ePuzzle = [[3, 4, 0, 0] , [2, 0, 3, 0] , [0, 3, 0, 2] , [0, 0, 1, 3]]
-- Below 2 lines send the data to the hOrN (headOrNothing) function. 
-- In example, if there are no values then it will show nothing otherwise it will show the right output(4x4).
solved :: B -> Maybe B
solved = hOrN . solution
-- Returns all the solutions. Now all the numbers will be checked and 0 number will be picked. Now it will check one by one row and put all the numbers between 1 to 4.
-- if the first row is filled with all the numbers then it will check the columns.
solution :: B -> [B]
solution z = solution1 (eLocation z) z where
    solution1 :: [L] -> B -> [B]
    solution1 []     z = [z]
    solution1 (x:ab) z = concatMap (solution1 ab) candidateB where
        candidateNumber  = [c | c <- [1..4], possibleNumber c x z]
        candidateB = map (\c -> copyNumber c x z) candidateNumber
-- It sends back the list of numbers where value is 0.
eLocation :: B -> [L]
eLocation z = [(row, col) | row <- [0..3], col <- [0..3], z ! (row, col) == 0]
-- Now this function checks if the number is at correct place or not. 
possibleNumber :: Number -> L -> B -> Bool
possibleNumber c (row, col) z = notInrow && notIncolumn && notInbox where
    notInrow    = notElem c $ z `numberR` row
    notIncolumn = notElem c $ z `numberC` col
    notInbox    = notElem c $ z `number4x4` (row, col)
-- Returns the board with specified value at specified Location.
copyNumber :: Number -> L -> B -> B
copyNumber mark (row, col) z = z // [((row, col), mark)]
-- Sends the numbers in the specific row.
numberR :: B -> Int -> [Number]
z `numberR` row = [z ! l | l <- range((row, 0), (row, 3))]
-- Sends the numbers in the specified column.
numberC ::  B -> Int -> [Number]
z `numberC` col = [z ! l | l <- range((0, col), (3, col))]
-- Sends the numbers in the 4x4 box that includes the specific Location.
number4x4 :: B -> L -> [Number]
z `number4x4` (row, col) = [z ! l | l <- location] where
    row' = (row `div` 2) * 2
    col' = (col `div` 2) * 2
    location = range((row', col'), (row' + 1, col' + 1))
pAssocs :: [[Number]] -> [(L, Number)]
pAssocs = concatMap rowA . zip [0..3] where
    rowA :: (Int, [Number]) -> [((Int, Int), Number)]
    rowA (row, numbers) = colA row $ zip [0..3] numbers
    colA :: Int -> [(Int, Number)] -> [((Int, Int), Number)]
    colA row cols = map (\(col, m) -> ((row, col), m)) cols
-- In this function if there is nothing on ePuzzle then it prints the line 66. If in ePuzzle, the numbers available then print the line 67.
hOrN :: [k] -> Maybe k
hOrN []     = Nothing
hOrN (x:ab) = Just x
-- Print the Puzzle.
printBoard :: Maybe B -> IO ()
printBoard Nothing  = putStrLn "No solution"
printBoard (Just z) = mapM_ putStrLn [show $ z `numberR` row | row <- [0..3]]
-- To run the programe 
-- runhaskell task2.hs (filename.hs)
-- Reference 
-- https://www.haskell.org/onlinereport/haskell2010/
-- https://www.haskell.org/onlinereport/haskell2010/haskellch14.html#x22-20100014
-- https://www.haskell.org/onlinereport/haskell2010/haskellch20.html#x28-24600020.9
-- https://wiki.haskell.org/Sudoku ( for sreference how to solve)
-- https://stackoverflow.com/    (for Error)