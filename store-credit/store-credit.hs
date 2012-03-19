{-# OPTIONS_GHC -i.. #-}

{-
 - URL: http://code.google.com/codejam/contest/351101/dashboard#s=p0
 -}

import CodeJam

data Item = Item { index :: Int, price :: Int } deriving (Show)

main :: IO ()
main = do
    input <- fmap lines getContents
    mapM_ putStrLn $ map (show.(solveCase solve)) $ getCases input

linesPerCase :: Int
linesPerCase = 3
    
solve :: Input -> String
solve input = formatAnswer res1 res2
    where (l1:l2:l3:_) = input
          credit = read l1 :: Int
          items = read l2 :: Int
          prices = map read $ take items $ (words l3) :: [Int]
          pricelist = zipWith Item [1..] prices
          options = [(item1,item2) | item1 <- pricelist, item2 <- pricelist,
                                    (index item1) /= (index item2),
                                    isValid credit (item1,item2)]
          (res1,res2) = head options

isValid :: Int -> (Item,Item) -> Bool
isValid credit (item1,item2) = (price item1) + (price item2) == credit

formatAnswer :: Item -> Item -> String
formatAnswer item1 item2 = (show lowIndex) ++ " " ++ (show highIndex)
    where i1 = index item1
          i2 = index item2
          lowIndex = min i1 i2
          highIndex = max i1 i2
