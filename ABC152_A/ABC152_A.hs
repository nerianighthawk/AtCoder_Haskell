toInt :: (String -> Int)
toInt x = read x ::Int


getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y


check :: Int -> Int -> String
check n m
    | n == m = "Yes"
    | otherwise = "No"


main = do
    x <- getInts
    let n = head x
    let m = last x
    let r = check n m
    putStrLn r

