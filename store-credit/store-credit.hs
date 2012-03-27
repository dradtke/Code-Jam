{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/351101/dashboard#s=p0
 -}

import CodeJam

data Item = Item { index :: Int, price :: Int }

instance Show Item where
    show (Item index price) = show index

main :: IO ()
main = codeJam solve

solve :: Input -> String
solve input = (show res1) ++ " " ++ (show res2)
    where (l1:l2:l3:_) = input
          credit = read l1 :: Int
          items = read l2 :: Int
          prices = map read $ take items $ (words l3) :: [Int]
          itemlist = zipWith Item [1..] prices
          options = [(item1,item2) | item1 <- itemlist, item2 <- itemlist,
                                     (index item1) < (index item2),
                                     (price item1) + (price item2) == credit]
          (res1,res2) = head options
