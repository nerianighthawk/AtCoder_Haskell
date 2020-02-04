toInt :: (String -> Int)
toInt x = read x ::Int


getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y


ten :: Int -> Int
ten x = x * 10


create :: Int -> Int -> Int
create n m =
    let s = take m (iterate ten n)
        r = sum s
    in r


ret :: Int -> Int -> Int
ret n m
    | n < m = create n m
    | m < n = create m n
    | otherwise = create n m


main = do
    x <- getInts
    let n = head x
    let m = last x
    let r = ret n m
    putStrLn $ show r
