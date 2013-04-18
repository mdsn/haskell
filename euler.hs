import Lists
-- problem 1

problem1 :: Int -> Int
problem1 n = sum . mergeuniq $ [
    map (*k) $ takeWhile (\x -> x*k < n) [1..] | k <- [3,5]]

