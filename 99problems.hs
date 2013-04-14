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
rle (x:xs) = let (first, rest) = span (==x) xs
              in (1 + count first, x) : rle rest
                where
                    count (x:xs) = 1 + count xs
                    count [] = 0

rle' :: (Eq a) => [a] -> [(Int, a)]
rle' [] = []
rle' xs = [(length x, head x) | x <- pack' xs]


