module Golf where

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

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) =
    if (y > x) && (y > z) then y : localMaxima (y:z:xs)
                          else localMaxima (y:z:xs)
localMaxima _      = []