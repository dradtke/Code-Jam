{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/90101/dashboard#s=p0
 -}

import CodeJam

type Pattern = [String]

main = do
    input <- fmap lines getContents
    let (l:d:n:_) = map read $ words $ head input :: [Int]
    let (knownWords,patterns) = splitAt d $ tail input
    let cases = makeCases $ map (:[]) patterns
    codeJam' (solve knownWords) cases

solve :: Input -> Input -> String
solve knownWords patterns = show $ foldl f 0 knownWords
    where pattern = convertPattern $ head patterns
          f acc word = if word `isMatch` pattern then acc + 1 else acc

convertPattern :: String -> Pattern
convertPattern [] = []
convertPattern p@(x:xs) =
    if [x] == "("
        then let (a,b) = extractGroup p in a : convertPattern b
        else [x] : convertPattern xs

extractGroup :: String -> (String,String)
extractGroup pattern = (group,rest)
    where pattern' = tail pattern
          group = takeWhile (\x -> [x] /= ")") pattern'
          rest = drop (length group + 1) pattern'

isMatch :: String -> Pattern -> Bool
isMatch [] _ = True
isMatch (x:xs) (y:ys) = if x `elem` y then isMatch xs ys else False
