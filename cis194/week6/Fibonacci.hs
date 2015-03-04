
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

data Stream a = Elem a (Stream a)

instance Show a => Show (Stream a) where
    show = unwords . map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Elem x s) = x : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Elem x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Elem x s) = Elem (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let x' = f x in Elem x $ streamFromSeed f x'

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Elem x xs) (Elem y ys) = Elem x $ Elem y $ interleaveStreams xs ys

-- Streams

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined