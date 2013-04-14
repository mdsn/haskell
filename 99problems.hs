-- problem 1

last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs

-- problem 2

nextolast' :: [a] -> a
nextolast' [] = error "Need at least two items"
nextolast' [x,_] = x
nextolast' (_:xs) = nextolast' xs

-- problem 3

elem' :: [a] -> Int -> a
elem' [] _ = error "Empty list"
elem' (x:_) 1 = x
elem' (_:xs) n
    | n < 1 = error "Index out of bounds"
    | otherwise = elem' xs (n-1)

-- problem 4

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- problem 5
--reverse' :: [a] -> [a]
--reverse' [] = []
--reverse' (x:xs) = reverse' xs ++ [x]
-- Better version:

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []
    where
        reverse'' [] reversed = reversed
        reverse'' (x:xs) reversed = reverse'' xs (x:reversed)

-- problem 6

palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome (x:xs) = x == last xs && palindrome (init xs)

-- problem 7

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- problem 8

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress' xs x
    where
        compress' [] _ = []
        compress' (x:xs) ignore
            | x == ignore = compress' xs ignore
            | otherwise = x : compress' xs x

-- problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' xs [[x]]
    where
        pack' [] ys = ys
        pack' (x:xs) ys
            | x == (head . last $ ys) = pack' xs [x:head ys]
            | otherwise = ys ++ pack' xs [[x]]

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack' rest

-- problem 10
-- run-length encoding

rle :: (Eq a) => [a] -> [(Int, a)]
rle [] = []
rle xs@(x:_) = let (first, rest) = span (==x) xs
                in (length first, x) : rle rest

rle' :: (Eq a) => [a] -> [(Int, a)]
rle' [] = []
rle' xs = [(length x, head x) | x <- pack' xs]

-- problem 11

data RleElem a = Single a | Multiple Int a
    deriving (Show)

rle'' :: (Eq a) => [a] -> [RleElem a]
rle'' [] = []
rle'' xs@(x:xs')
    | headLength == 1 = (Single x) : rle'' xs'
    | otherwise = (Multiple headLength x) : rle'' rest
    where
        headLength = length . fst $ split
        rest = snd split
        split = span (==x) xs

rle''' :: (Eq a) => [a] -> [RleElem a]
rle''' [] = []
rle''' xs@(x:_) =
    let (head', rest) = span (==x) xs
        hlength = length head'
    in  (if hlength == 1 then (Single x)
                         else (Multiple hlength x)) : rle''' rest

-- problem 12

rleDecode :: [RleElem a] -> [a]
rleDecode = concatMap decode'
    where
        decode' (Single x) = [x]
        decode' (Multiple n x) = replicate n x

-- problem 13

directRle :: (Eq a) => [a] -> [RleElem a]
directRle [] = []
directRle (x:xs) = directRle' xs x 1
    where
        build n x = if n > 1 then (Multiple n x) else (Single x)
        directRle' [] x n = [build n x]
        directRle' (x:xs) x' n
            | x == x' = directRle' xs x' (n+1)
            | otherwise = (build n x') : directRle' xs x 1
