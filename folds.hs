-- 1. sum squares of first n integers
sumsq :: Int -> Int
sumsq n = foldr (\x acc -> acc + x*x) 0 [1..n]

-- 2. define length
length' :: [a] -> Int
length' xs = foldr (\x acc -> acc + 1) 0 xs 

-- 3. define minlist (min from list) using foldr1
minlist :: (Ord a) => [a] -> a
minlist = foldr1 (min)

-- 4. define reverse
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- 5. define remove (ys - xs)
remove :: (Eq a) => [a] -> [a] -> [a]
remove xs ys = foldr (pick) [] ys
    where
        pick y acc
            | y `elem` xs = acc
            | otherwise = y:acc

-- 6. define filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (pick) []
    where
        pick x acc
            | p x = x:acc
            | otherwise = acc

-- 7. remdups (compress) - remove adjacent duplicates
compress :: (Eq a) => [a] -> [a]
compress = foldr (pick) []
    where
        pick x [] = [x]
        pick x acc
            | x == head acc = acc
            | otherwise = x:acc
