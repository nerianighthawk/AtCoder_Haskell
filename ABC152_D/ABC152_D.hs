toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
    x <- getLine
    let y = (map$toInt)$words x
    return y

check :: Int -> Int -> Bool
check x y = do
    let xshisu = floor (logBase (fromIntegral 10) (fromIntegral x))
    let xa = floor $ (fromIntegral x) / (fromIntegral (10 ^ xshisu))
    let xb = mod x 10
    let yshisu = floor (logBase (fromIntegral 10) (fromIntegral y))
    let ya = floor $ (fromIntegral y) / (fromIntegral (10 ^ yshisu))
    let yb = mod y 10
    (xa == yb) && (ya == xb)

res :: [Int] -> [Int] -> Int -> Int
res xs ys c
    | null xs = c
    | otherwise = res (tail xs) ys (c + length (filter (check (head xs)) ys))

main = do
    ns <- getInts
    let n = head ns
    let r = res (take n [1..]) (take n [1..]) 0
    putStrLn $ show r
    