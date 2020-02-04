toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y

a :: Int -> Int
a x = 2 * x

ret :: [Int] -> Int -> Int -> Int
ret hs c h
    | head hs > h = (head hs) - 1
    | otherwise = ret (tail hs) (c + 1) h

main = do
    ns <- getInts
    let h = head ns
    let r = ret (iterate a 1) 0 h
    putStrLn $ show r
