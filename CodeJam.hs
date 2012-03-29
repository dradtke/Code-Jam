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
--
-- This only works if input for the problem begins with one line containing
-- the number of cases followed by the input for each case. Some problems
-- require more specialized input (such as alien language); in those cases,
-- an alternate version of the method is provided named codeJam':
--
--   main :: IO ()
--   main = do
--       <read input here>
--       let cases = makeCases inputs
--       codeJam' solve cases
--
-- where inputs is a list containing the input (a list of strings) for each
-- case, one per element.

module CodeJam
( Input
, Case
, makeCases
, codeJam
, codeJam'
) where


-------------------------------- Public ---------------------------------------

-- For convenience, declare Input as a list of strings
type Input = [String]

-- Datatype representing a case
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

-- Out of a list of inputs, construct a list of unsolved cases
makeCases :: [Input] -> [Case]
makeCases inputs = zipWith Unsolved [1..] inputs

-- Entry point. You should call this from main
codeJam :: (Input -> String) -> IO ()
codeJam solve = do
    input <- fmap lines getContents
    mapM_ putStrLn $ map (show.(solveCase solve)) $ getCases input

-- Entry point for problems where case construction needs to be done manually
codeJam' :: (Input -> String) -> [Case] -> IO ()
codeJam' solve cases =
    mapM_ putStrLn $ map (show.(solveCase solve)) cases

-------------------------------- Private --------------------------------------

-- Define how to show a case. Used for displaying the final answer
instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

-- Given the full input, returns a list of cases for it
getCases :: Input -> [Case]
getCases input = makeCases $ splitIntoCases linesPerCase input'
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
