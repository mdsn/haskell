module Lists
( selections
, mergeuniq
) where

import Data.List (sort)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (y:[]) = [y]
intersperse x (y:ys) = y:x:intersperse x ys 

intercalate :: [a] -> [[a]] -> [a]
intercalate x (y:[]) = y
intercalate x (y:ys) = y ++ x ++ intercalate x ys

-- ??
--transpose :: [[a]] -> [[a]]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

selections :: [a] -> [(a,[a])]
selections [] = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

-- merge sorted lists, discard repeated
-- e.g. mergeuniq [[1,2,3], [3,4,5], [2,6,7]] -> [1,2,3,4,5,6,7]
mergeuniq :: (Eq a, Ord a) => [[a]] -> [a]
mergeuniq = uniq . sort . concat
    where
        uniq [] = []
        uniq (x:[]) = [x]
        uniq (x:xs)
            | x /= (head xs) = x : uniq xs
            | otherwise = uniq xs
