-- A module containing some common utilities for solving Code Jam problems
module CodeJam where

type Input = [String]
data Case = Unsolved { number :: Int , input :: Input }
            | Solved { number :: Int , answer :: String }

instance Show Case where
    show (Unsolved number input) = "Case #" ++ (show number) ++ ": N/A"
    show (Solved number answer) = "Case #" ++ (show number) ++ ": " ++ answer

codeJam :: (Input -> String) -> IO ()
codeJam solve = do
    input <- fmap lines getContents
    mapM_ putStrLn $ map (show.(solveCase solve)) $ getCases input

getCases :: Input -> [Case]
getCases input = zipWith Unsolved [1..] $ splitIntoCases input' linesPerCase
    where n = read $ head input :: Int
          input' = tail input
          linesPerCase = div (length input') n

splitIntoCases :: Input -> Int -> [Input]
splitIntoCases input n
    | post == [] = [pre]
    | otherwise = pre:(splitIntoCases post n)
    where (pre,post) = splitAt n input

solveCase :: (Input -> String) -> Case -> Case
solveCase solve c = case c of
    Unsolved number input -> Solved number $ solve input
    _ -> c
