-- A module containing some common utilities for solving Code Jam problems.
-- To solve a Code Jam problem, use
--
--   main :: IO ()
--   main = codeJam' solve
--
--   solve :: Input -> String
--   solve input = ...
--
-- where 'solve' takes the input (a list of strings) for one case and returns
-- that case's result as a string.
--
-- This only works if each case requires the same number of input lines.
-- Some problems require more specialized input, and they can be solved
-- like this:
--
--   main :: IO ()
--   main = do
--       let input = <read input here>
--       codeJam input solve
--
-- Remember, input is expected as a list of Input's, which is equivalent
-- to the type [[String]].
--
-- Both codeJam and codeJam' take care of reading the input, executing
-- the method to solve it, and printing it to standard output.

module CodeJam
( Input
, codeJam
, codeJam'
) where


-------------------------------- Public ---------------------------------------

-- For convenience, declare Input as a list of strings
type Input = [String]

-- Entry point for problems that follow the standard input format
codeJam' :: (Input -> String) -> IO ()
codeJam' solve = do
    input <- fmap (convertInput.lines) getContents
    codeJam input solve

-- Takes a list of inputs, a solve method, and solves the problem
codeJam :: [Input] -> (Input -> String) -> IO ()
codeJam input solve =
    mapM_ putStrLn $ map (show.(solveCase solve)) $ makeCases input

-------------------------------- Private --------------------------------------

-- Datatype representing a case
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

-- Define how to show a case. Used for displaying the final answer
instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

-- Out of a list of inputs, construct a list of unsolved cases
makeCases :: [Input] -> [Case]
makeCases inputs = zipWith Unsolved [1..] inputs

-- Programmatically determines the input for each case
convertInput :: Input -> [Input]
convertInput input = splitIntoCases linesPerCase input'
    where n = read $ head input :: Int
          input' = tail input
          linesPerCase = div (length input') n

-- Splits input into a list of smaller inputs, one for each case
splitIntoCases :: Int -> Input -> [Input]
splitIntoCases n input
    | post == [] = [pre]
    | otherwise = pre:(splitIntoCases n post)
    where (pre,post) = splitAt n input

-- Uses the provided method to solve a case
solveCase :: (Input -> String) -> Case -> Case
solveCase solve c = case c of
    Unsolved number input -> Solved number $ solve input
    _ -> c
