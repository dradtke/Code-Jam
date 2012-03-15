{-
 - URL: http://code.google.com/codejam/contest/dashboard?c=351101#s=p1
 -}

data Case = Case { number :: Int, input :: String } deriving (Show)

main :: IO ()
main = do
    input <- fmap lines getContents
    let n = read $ head input :: Int
    let cases = zipWith Case [1..] $ take n (tail input)
    mapM_ putStrLn $ map solveCase cases

solveCase :: Case -> String
solveCase c = "Case #" ++ (show $ number c) ++ ": " ++ ans
    where ans = unwords.reverse.words $ input c
