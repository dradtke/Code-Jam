{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

import CodeJam

main :: IO ()
main = codeJam solve

solve :: Input -> String
solve input = unwords.reverse.words $ head input
