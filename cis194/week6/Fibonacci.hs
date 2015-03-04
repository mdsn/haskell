
module Week6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = let xs  = 0 : 1 : rest xs in xs
  where
    rest []         = []
    rest [_]        = []
    rest (x1:x2:rs) = (x1 + x2) : rest (x2:rs)
