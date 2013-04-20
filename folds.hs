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

-- 8. define inits
inits :: [a] -> [[a]]
inits xs = reverse (foldr (newInit) [] (reverse xs))
    where
        newInit x [] = [[x], []]
        newInit x acc = (head acc ++ [x]):acc

-- 9. define approxe n = sum (1/i!), from i = 0 to n - using foldl
approx :: (Fractional a, Enum a) => a -> a -> a
approx a i = a + 1 / f i
    where
        f 0 = 1
        f n = foldr (*) 1 [1..n]

approxe :: (Fractional a, Enum a) => a -> a
approxe n = foldl (approx) 0 [0..n]

-- 10. define sae -> successive approximations to e
sae :: (Fractional a, Enum a) => a -> [a]
sae n = scanl (approx) 1 [1..n]
