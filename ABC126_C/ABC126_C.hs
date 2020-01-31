import Data.List

toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y

ret :: [Int] -> Int -> Int -> Int
ret hs n k
    | n <= k = 0
    | otherwise = sum (drop k (reverse $ Data.List.sort hs))

main = do
    ns <- getInts
    let n = head ns
    let k = last ns
    hs <- getInts
    let r = ret hs n k
    putStrLn $ show r