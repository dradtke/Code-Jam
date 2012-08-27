{-
 - URL: http://code.google.com/codejam/contest/90101/dashboard#s=p0
 -}

import Control.Monad
type Pattern = [String]

main = do
    -- Read the first three variables: L (word length),
    -- D (number of known words), and N (number of test cases)
    (l:d:n:_) <- readIntList
    
    -- Read in the next D lines, which contain the known words
    knownWords <- getLines d
    
    -- Solve
    mapM (solve knownWords) [1..n]

-- Solve the problem, given a list of known words and the case number
solve :: [String] -> Int -> IO ()
solve knownWords i = do
    -- Read in the pattern and convert it
    pattern <- liftM convertPattern getLine
    
    -- Iterate over the known words, comparing the pattern to each one
    let ans = countMatches knownWords pattern
    putStrLn $ "Case #" ++ show i ++ ": " ++ show ans
    
-- Count the number of known words that match the pattern
countMatches :: [String] -> Pattern -> Int
countMatches knownWords pattern = foldl f 0 knownWords
    where f acc word = if word `isMatch` pattern then acc+1 else acc

-- Create a pattern from a string. Each element of a pattern is a list
-- of characters that are valid in that position
convertPattern :: String -> Pattern
convertPattern [] = []
convertPattern p@(x:xs) =
    if [x] == "("
        then let (a,b) = extractGroup p in a : convertPattern b
        else [x] : convertPattern xs

-- Extract a parenthetical group from a string, used for creating patterns
extractGroup :: String -> (String,String)
extractGroup pattern = (group,rest)
    where pattern' = tail pattern
          group = takeWhile (\x -> [x] /= ")") pattern'
          rest = drop (length group + 1) pattern'

-- Returns true if the string matches the pattern
isMatch :: String -> Pattern -> Bool
isMatch [] _ = True
isMatch (x:xs) (y:ys) = if x `elem` y then isMatch xs ys else False

-- Read a list of Int's from standard input
readIntList :: IO [Int]
readIntList = getLine >>= \l -> return (fmap read $ words l)

-- Get the next n lines from standard input
getLines :: Int -> IO [String]
getLines n = foldM f [] [1..n]
    where f l _ = getLine >>= \x -> return $ x:l  
