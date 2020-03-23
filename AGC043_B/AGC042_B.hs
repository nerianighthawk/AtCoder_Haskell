toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
    x <- getLine
    let y = (map$toInt)$x
    return y

main = do
    n <- getInts
    as <- getInts
    let h = head ns
    let r = ret (iterate a 1) 0 h
    putStrLn $ show r