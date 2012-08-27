{-
 - URL: http://code.google.com/codejam/contest/351101/dashboard#s=p0
 -}

import Control.Monad

-- Read in the number of cases and call 'solve' for each one
main = readLn >>= \n -> mapM solve [1..n]

-- Solve a case, passing in the case number
solve :: Int -> IO ()
solve i = do
    -- Read in the variables for this case
    credit <- readLn :: IO (Int)
    items  <- readLn :: IO (Int)
    prices <- readIntList
    
    -- Create a list of item tuples, which associates index with price
    let itemList = zip [1..] prices
    
    -- Use a list comprehension to find all item pairs whose prices
    -- add up to the available credit. Since we know there will be
    -- one and only one, we can take the head of the list and extract
    -- the indices of each item
    let ((j,_),(k,_)) = head $ [(a,b) | a <- itemList, b <- itemList,
                        (fst a) < (fst b), (snd a) + (snd b) == credit]
                                        
    -- Display the result
    putStrLn $ "Case #" ++ show i ++ ": " ++ show j ++ " " ++ show k
    
-- Read a list of Int's from standard input
readIntList :: IO [Int]
readIntList = getLine >>= \l -> return (fmap read $ words l)
