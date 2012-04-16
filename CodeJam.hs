-- A module containing some common utilities for solving Code Jam problems.
-- Includes some improvements provided by Abizer Nasir (https://github.com/Abizern/Code-Jam)
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
--       let inputs = <read input here>
--       codeJam solve inputs
--
-- Remember, inputs is expected to be a list of Inputs, which is equivalent
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
  inputs <- fmap (convertInput.lines) getContents
  codeJam solve inputs

-- Takes a list of inputs, a solve method, and solves the problem
codeJam :: (Input -> String) -> [Input] -> IO ()
codeJam solve inputs =
  mapM_ putStrLn $ zipWith (++) prefixes $ map solve inputs

-------------------------------- Private --------------------------------------

-- Programmatically determines the input for each case
convertInput :: Input -> [Input]
convertInput input = splitIntoCases linesPerCase input'
  where n            = read $ head input :: Int
        input'       = tail input
        linesPerCase = div (length input') n

-- Splits input into a list of smaller inputs, one for each case
splitIntoCases :: Int -> Input -> [Input]
splitIntoCases n input
  | post == []  = [pre]
  | otherwise   = pre:(splitIntoCases n post)
  where (pre,post) = splitAt n input

-- Provides the boilerplate prefixes for the output
prefixes :: [String]
prefixes = ["Case #" ++ show n ++ ": " | n <- [1..]]
