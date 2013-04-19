-- 1. sum squares of first n integers
sumsq :: Int -> Int
sumsq n = foldr (\x acc -> acc + x*x) 0 [1..n]

-- 2. define length with foldr and foldl
length' :: [a] -> Int
length' xs = foldr (\x acc -> acc + 1) 0 xs 
