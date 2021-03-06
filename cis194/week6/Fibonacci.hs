{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-methods #-}

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

-- A simple cons operator, similar to :
infixr 5 .+
(.+) :: a -> Stream a -> Stream a
x .+ s = Stream x s

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = x .+ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = f x .+ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let x' = f x in x .+ streamFromSeed f x'

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = x .+ interleaveStreams ys xs

-- Streams

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr1 interleaveStreams . streamToList . streamMap streamRepeat $ nats

x :: Stream Integer
x = 0 .+ 1 .+ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n                     = n .+ streamRepeat 0
    negate                            = streamMap (* (-1))
    (Stream x xs) + (Stream y ys)     = (x + y) .+ (xs + ys)
    (Stream x xs) * ies@(Stream y ys) = (x * y) .+ (streamMap (*x) ys + (xs * ies))

instance Fractional (Stream Integer) where
    s1@(Stream x xs) / s2@(Stream y ys) = q
      where
        q = (x `div` y) .+ streamMap (`div` y) (xs - (s1 / s2) * ys)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

{-
    a1 a2       b1 b2
    a3 a4       b3 b4
-}

instance Num Matrix where
    (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
        Matrix (a1*b1 + a2*b3) (a1*b2 + a2*b4)
               (a3*b1 + a4*b3) (a3*b2 + a4*b4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case (Matrix 1 1 1 0)^n of Matrix _ fn _ _ -> fn