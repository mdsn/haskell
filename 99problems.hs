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


