positiveSum :: [Int] -> Int
positiveSum [] = 0
positiveSum [x] = x if x > 0 else 0
positiveSum (x:xs)
    | x > 0 = x
    | otherwise = 0