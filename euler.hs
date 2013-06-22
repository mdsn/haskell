import Data.List (nub, sort)

problem1 :: Int
problem1 = sum . nub . sort $ [3, 6..1000] ++ [5, 10..995]
 

problem2 :: Int
problem2 = let n = 4000000 in sum . filter (even) . takeWhile (< n) $ f
    where
        f = 1 : 1 : zipWith (+) f (tail f)

problem4 :: Int
problem4 = maximum . map read . filter p $
    [show (x*y) | x <- [999,998..100], y <- [999,998..100]]
    where
        p x = x == reverse x
