
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

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = unwords . map show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let x' = f x in Stream x $ streamFromSeed f x'

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x $ interleaveStreams ys xs

-- Streams

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr1 interleaveStreams . streamToList . streamMap streamRepeat $ nats
