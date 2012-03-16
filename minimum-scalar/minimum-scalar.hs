{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/32016/dashboard#s=p0
 -}

import CodeJam
import Data.List

main :: IO ()
main = do
    input <- fmap lines getContents
    let n = read $ head input :: Int
    let cases = zipWith Unsolved [1..] $ breakIntoCases (tail input) n 3
    mapM_ putStrLn $ map (show.(solveCase solve)) cases

solve :: Input -> String
solve input = show result
    where (l1:l2:l3:_) = input
          n = read l1 :: Int
          v1 = parseVector l2 n
          v2 = parseVector l3 n
          result = scalar (sort v1) (reverse $ sort v2)

parseVector :: String -> Int -> [Integer]
parseVector line n = map read $ take n $ (words line) :: [Integer]

scalar :: [Integer] -> [Integer] -> Integer
scalar v1 v2 = sum $ zipWith (*) v1 v2 :: Integer
