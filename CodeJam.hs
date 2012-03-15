-- A module containing some common utilities for solving Code Jam problems
module CodeJam where

type Input = [String]
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

breakIntoCases :: Input -> Int -> Int -> [Input]
breakIntoCases input n count
    | n <= 1 = [input]
    | otherwise = (take count input)
                : breakIntoCases (drop count input) (n - 1) count

