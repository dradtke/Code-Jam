{-
 - URL: http://code.google.com/codejam/contest/351101/dashboard#s=p0
 -}

data Item = Item { index :: Int, price :: Int }
data Case = Case { number :: Int, input :: [String] }

main :: IO ()
main = do
    input <- fmap lines getContents
    let cases = zipWith Case [1..] $ breakIntoCases (tail input) 3
    let results = map solveCase cases
    mapM_ putStrLn results
    
-- | Turns a list of input lines into a list where each element
-- | contains the lines necessary for just that case.
breakIntoCases :: [String] -> Int -> [[String]]
breakIntoCases [] _ = []
breakIntoCases input count =
    (take count input) : breakIntoCases (drop count input) count

solveCase :: Case -> String
solveCase c = "Case #" ++ (show $ number c) ++ ": " ++ ans
    where (l1:l2:l3:_) = input c
          credit = read l1 :: Int
          prices = map read $ (words l3) :: [Int]
          pricelist = zip [1..] prices
          options = [(p1,p2) | p1 <- pricelist, p2 <- pricelist, p1 /= p2]
          ((i1,_),(i2,_)) = head $ filter (isValid credit) options
          ans = (show $ min i1 i2) ++ " " ++ (show $ max i1 i2)

isValid :: Int -> ((Int,Int),(Int,Int)) -> Bool
isValid credit ((i1,p1),(i2,p2)) = p1 + p2 == credit
