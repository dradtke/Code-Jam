-- A module containing some common utilities for solving Code Jam problems.
-- To solve a Code Jam problem, use
--
--   main :: IO ()
--   main = codeJam solve
--
--   solve :: Input -> String
--   solve input = ...
--
-- where 'solve' takes the input (a list of strings) for one case and returns
-- that case's result as a string.

module CodeJam where

-- For convenience, declare Input as a list of strings
type Input = [String]

-- Datatype representing a case
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

-- Define how to show a case. Used for displaying the final answer
instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

-- Entry point. You should call this from main
codeJam :: (Input -> String) -> IO ()
codeJam solve = do
    input <- fmap lines getContents
    mapM_ putStrLn $ map (show.(solveCase solve)) $ getCases input

-- Given the full input, returns a list of cases for it
getCases :: Input -> [Case]
getCases input = zipWith Unsolved [1..] $ splitIntoCases input' linesPerCase
    where n = read $ head input :: Int
          input' = tail input
          linesPerCase = div (length input') n

-- Splits input into a list of smaller inputs, one for each case
splitIntoCases :: Input -> Int -> [Input]
splitIntoCases input n
    | post == [] = [pre]
    | otherwise = pre:(splitIntoCases post n)
    where (pre,post) = splitAt n input

-- Uses the provided method to solve a case
solveCase :: (Input -> String) -> Case -> Case
solveCase solve c = case c of
    Unsolved number input -> Solved number $ solve input
    _ -> c
