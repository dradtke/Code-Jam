{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

import Control.Monad

-- Read in the number of cases and call 'solve' for each one
main = readLn >>= \n -> mapM solve [1..n]
    
-- Solve a case, passing in the case number
solve :: Int -> IO ()
solve i = do
    -- Read in the next line
    line <- getLine
    
    -- Convert the line to a list of words, reverse it, then
    -- convert it back to a string
    let ans = (unwords.reverse.words) line
    
    -- Print the answer
    putStrLn $ "Case #" ++ show i ++ ": " ++ ans
