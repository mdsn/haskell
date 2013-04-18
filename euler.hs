import Data.List (nub, sort)

-- problem 1

problem1 :: Int -> Int
problem1 n = sum . merge $ [
    map (*k) $ takeWhile (\x -> x*k < n) [1..] | k <- [3,5]]
    where
        merge = nub . sort . concat

