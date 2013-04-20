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

-- 11. define iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = scanl (\a x -> f a) x [1..]

-- 12. define shift [abc] -> [bca], define rotate (all rotations of a list)
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotate :: (Eq a) => [a] -> [[a]]
rotate xs = foldl (rotate') [] xs
    where
        rotate' [] x = [xs]
        rotate' (h:acc) x = (shift h):h:acc

-- 13. define mult using add and pred
--            exp using mult and pred
--            tetration using exp and pred
add i 0 = i
add i j = succ . add i . pred $ j

mult :: (Num a, Enum a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult i 1 = i
mult i j = (+i) . mult i $ (pred j)

exp' :: (Num a, Enum a) => a -> a -> a
exp' i 1 = i
exp' i j = (*i) . exp' i $ (pred j)

tetration :: (Num a, Enum a) => a -> a -> a
tetration a 1 = a
tetration a n = exp' a . tetration a $ (pred n)

-- 13 b. define add, mult, exp using foldi
foldi :: (a -> a) -> a -> Int -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

add' :: Int -> Int -> Int
add' i j = foldi (+1) i j

mult' :: (Num a, Enum a) => a -> Int -> a
mult' i j = foldi (+i) 0 j

exp'' :: (Num a, Enum a) => a -> Int -> a
exp'' i j = foldi (*i) 1 j
