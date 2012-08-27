{-
 - URL: http://code.google.com/codejam/contest/32016/dashboard#s=p0
 -}

import Control.Monad
import Data.List

-- Read in the number of cases and call 'solve' for each one
main = readLn >>= \n -> mapM solve [1..n]

-- Solve a case, passing in the case number
solve :: Int -> IO ()
solve i = do
    -- Read in the number of items per vector. Not used,
    -- but we need to read it in order to get past it
    readLn :: IO (Int)
    
    -- Read each vector in as a list of Ints, then sort the
    -- first one and reverse-sort the second one
    v1 <- readIntList >>= \l -> return (sort l)
    v2 <- readIntList >>= \l -> return (reverse $ sort l)
    
    -- Calculate the scalar
    let result = scalar v1 v2
    
    -- Display the result
    putStrLn $ "Case #" ++ show i ++ ": " ++ show result

-- Read a list of Int's from standard input
readIntList :: IO [Int]
readIntList = getLine >>= \l -> return (fmap read $ words l)

-- Calculate the scalar value of two vectors. The input vectors
-- are first converted from [Int] to [Integer] to prevent potential
-- overflows
scalar :: [Int] -> [Int] -> Integer
scalar v1 v2 = sum $ zipWith (*) v1' v2'
    where v1' = map toInteger v1
          v2' = map toInteger v2
