{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

import CodeJam

main :: IO ()
main = do
    input <- fmap lines getContents
    mapM_ putStrLn $ map (show.(solveCase solve)) $ getCases input

linesPerCase :: Int
linesPerCase = 1

solve :: Input -> String
solve input = unwords.reverse.words $ head input
