{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/351101/dashboard#s=p0
 -}

import CodeJam

data Item = Item { index :: Int, price :: Int } deriving (Show)

main :: IO ()
main = codeJam solve

solve :: Input -> String
solve input = (show $ index res1) ++ " " ++ (show $ index res2)
    where (l1:l2:l3:_) = input
          credit = read l1 :: Int
          items = read l2 :: Int
          prices = map read $ take items $ (words l3) :: [Int]
          pricelist = zipWith Item [1..] prices
          options = [(item1,item2) | item1 <- pricelist, item2 <- pricelist,
                                    (index item1) < (index item2),
                                    isValid credit (item1,item2)]
          (res1,res2) = head options

isValid :: Int -> (Item,Item) -> Bool
isValid credit (item1,item2) = (price item1) + (price item2) == credit
