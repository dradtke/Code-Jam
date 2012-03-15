{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

import CodeJam

main :: IO ()
main = do
    input <- fmap lines getContents
    let n = read $ head input :: Int
    let cases = zipWith Unsolved [1..] $ breakIntoCases (tail input) n 1
    mapM_ putStrLn $ map (show.(solveCase solve)) cases

solve :: Input -> String
solve input = unwords.reverse.words $ head input
