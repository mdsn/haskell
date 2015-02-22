module Golf where

import Control.Monad (liftM)
import Data.Function (on)
import Data.List (sortBy)

{- Skips -}

skips :: [a] -> [[a]]
skips [] = []
skips xs = [every n xs | n <- [1..length xs]]
  where
    {- every -
    Builds a list of (i, z) by zipping zs with an infinite list [1..n,1..n,...]
    and filters on those indexed equal to n (the ones we have to keep), finally
    getting rid of the indexes. -}
    every :: Int -> [a] -> [a]
    every n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

    {-
    every :: Int -> [a] -> [a]
    every _ [] = []
    every n ys = every' 1 ys
      where
        every' _       [] = []
        every' current (x:xs')
          | n == current = x : every' 1 xs'
          | otherwise    = every' (current + 1) xs'
    -}

{- Local maxima -}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) =
    if (y > x) && (y > z) then y : localMaxima (y:z:xs)
                          else localMaxima (y:z:xs)
localMaxima _      = []

{- Histogram -}

type Histogram a = [Freq a]

data Freq a = Freq { value :: a
                   , freq  :: Int } deriving (Show, Eq)

-- Comparison by value
instance (Eq a, Ord a) => Ord (Freq a) where
    compare a b = compare (value a) (value b)

sortByFreq :: Histogram a -> Histogram a
sortByFreq = sortBy (compare `on` freq)

maxByFreq :: Histogram a -> Maybe (Freq a)
maxByFreq [] = Nothing
maxByFreq xs = let (m:_) = reverse (sortByFreq xs)
                in Just m

maxFreq :: Histogram a -> Maybe Int
maxFreq xs = liftM freq (maxByFreq xs)

freqOf :: (Eq a) => a -> Histogram a -> Int
freqOf _ []                = 0
freqOf a xs
  | not (inHistogram a xs) = 0
  | otherwise              = let (x:_) = filter ((== a) . value) xs
                             in freq x

inHistogram :: (Eq a) => a -> Histogram a -> Bool
inHistogram _ []     = False
inHistogram a (x:xs) = (a == value x) || inHistogram a xs

incFreq :: Eq a => a -> Histogram a -> Histogram a
incFreq a []     = [Freq a 1]
incFreq a (x@(Freq v f):xs)
  | a == v    = Freq v (f+1) : xs
  | otherwise = x : incFreq a xs

buildHistogram :: Eq a => [a] -> Histogram a
buildHistogram = foldr incFreq []

line :: Int -> Histogram Int -> String
line 0 _  = ""
line n xs = [f x | x <- [0..9]]
  where
    f n' | inHistogram n' xs && freqOf n' xs >= n = '*'
         | otherwise                              = ' '

histLines :: Histogram Int -> String
histLines [] = ""
histLines xs = case maxFreq xs of Just f  -> lines' f
                                  Nothing -> ""
  where
    lines' :: Int -> String
    lines' 0           = "==========\n0123456789\n"
    lines' currentFreq = line currentFreq xs ++ "\n" ++ lines' (currentFreq-1)

histogram :: [Int] -> String
histogram [] = ""
histogram ns = histLines (buildHistogram ns)