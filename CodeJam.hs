-- A module containing some common utilities for solving Code Jam problems
module CodeJam where

type Input = [String]
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

splitIntoCases :: Input -> Int -> [Input]
splitIntoCases input n
    | post == [] = [pre]
    | otherwise = pre:(splitIntoCases post n)
    where (pre,post) = splitAt n input

solveCase :: (Input -> String) -> Case -> Case
solveCase solve c = case c of
    Unsolved number input -> Solved number $ solve input
    _ -> c
