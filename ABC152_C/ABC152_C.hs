toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y

ret :: [Int] -> Int -> Int -> Int
ret ps x max
    | null ps = x
    | (head ps) > max = ret (tail ps) x max
    | otherwise = ret (tail ps) (x + 1) (head ps)

main = do
    ns <- getInts
    let n = head ns
    ps <- getInts
    let r = ret ps 0 n
    putStrLn $ show r
