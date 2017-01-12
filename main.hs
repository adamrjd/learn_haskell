minus a@(x:xs) b@(y:ys) = case compare x y of
         LT -> x : minus  xs b
         EQ ->     minus  xs ys
         GT ->     minus  a  ys
minus a        b        = a 
primesTo m = eratos [2..m] where
   eratos (p : xs) 
      | p*p > m   = p : xs
      | otherwise = p : eratos (xs `minus` [p*p, p*p+p..m])

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x ` div` 10)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

consolidate :: Integral x => [x] -> x
consolidate [] = 0
consolidate x = sum [(10^i)*j | (i,j) <- zip [0..] (reverse x)]

takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x:xs) = x : takeStep n (drop (n-1) xs)

slice :: Int -> Int -> Int -> [a] -> [a]
slice start stop step = takeStep step . take (stop - start) . drop start

revR :: [Int] -> [Int]
revR [] = []
revR (x:xs) = revR xs ++ [x]

