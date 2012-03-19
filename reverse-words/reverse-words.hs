{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

import CodeJam

main :: IO ()
main = do
    input <- fmap lines getContents
    let n = read $ head input :: Int
    let input' = take (n * linesPerCase) $ tail input
    let cases = zipWith Unsolved [1..] $ splitIntoCases input' linesPerCase
    mapM_ putStrLn $ map (show.(solveCase solve)) cases

linesPerCase :: Int
linesPerCase = 1

solve :: Input -> String
solve input = unwords.reverse.words $ head input
