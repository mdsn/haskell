module Week1 where

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [digit]
  where
    digit = n `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . double . reverse
  where
    double []       = []
    double [n]      = [n]
    double (n:m:ns) = n : (m*2) : double ns

sumDigits :: [Integer] -> Integer
sumDigits = foldr add 0
  where
    add x acc = acc + sum (toDigits x)

validate :: Integer -> Bool
validate = isValid . sumDigits . doubleEveryOther . toDigits
  where
    isValid = (==0) . (`mod` 10)