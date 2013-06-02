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

length'' :: [a] -> Int
length'' [] = 0
length'' (_:xs) = length''' xs 1
    where
        length''' [] acc = acc
        length''' (_:xs) acc = length''' xs (acc+1)

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

-- problem 14

dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = x:x:dupl xs

-- problem 15

nupl :: Int -> [a] -> [a]
nupl n = concatMap (replicate n)

-- problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n
    where
        dropEvery' [] _ _ = []
        dropEvery' (x:xs) n m
            | m == 1 = dropEvery' xs n n
            | otherwise = x : dropEvery' xs n (m-1)

-- problem 17

split :: [a] -> Int -> ([a], [a])
split xs n
    | n > length xs = error "Index out of bounds"
    | otherwise = split' [] xs n
    where
        split' ys xs' 0 = (ys, xs')
        split' ys (x:xs') n = split' (ys ++ [x]) xs' (n-1)

split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n
    | n > 0 = let (f, l) = split' xs (n-1) in (x:f, l)
--    | otherwise = ([], xs)
split' xs _ = ([], xs)

-- problem 18

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ from to
    | to < from = error "Start index is greater than end index"
slice (x:xs) from to
    | to == 1 = [x]
    | from == 1 = x : slice xs 1 (to-1)
    | otherwise = slice xs (from-1) (to-1)

-- problem 19

rotate :: [a] -> Int -> [a]
rotate xs@(x:xs') n
    | n > 0 = rotate (xs' ++ [x]) (n-1)
    | n < 0 = rotate (last xs' : init xs) (n+1)
    | otherwise = xs

-- problem 20

removeAt :: Int -> [a] -> (a, [a])
removeAt 0 _ = error "Index is 1-based"
removeAt _ [] = error "Empty list"
removeAt n xs = removeAt' n xs []
    where
        removeAt' n (x:xs') acc
            | n == 1 = (x, acc ++ xs')
            | n > 1 = removeAt' (n-1) xs' (acc ++ [x])

-- problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

-- problem 22

range :: Int -> Int -> [Int]
range x y
    | x > y = []
    | x <= y = x:range (x+1) y

-- problem 26

combinations :: Int -> [a] -> [[a]]
combinations _ []     = [[]]
combinations 0 _      = []
combinations n (x:xs)
    | length xs < n   = [x:xs]
    | otherwise       = (map (x:) (combinations (n-1) xs)) ++
                        (combinations n xs)
