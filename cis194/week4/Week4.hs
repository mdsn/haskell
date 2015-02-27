module Week4 where

import Data.List (sort, partition)

fun1 :: [Integer] -> Integer
fun1 []      = 1
fun1 (x:xs)
 | even x    = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1             = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- ¿?
-- fun2' :: Integer -> Integer

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert :: a -> Tree a -> Tree a
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node d l v r)
      | leftHeight < rightHeight  = Node d (insert x l) v r
      | leftHeight > rightHeight  = Node d l v r'
      | leftHeight == rightHeight = Node (h+1) l v r'
      where
        leftHeight = height l
        rightHeight = height r

        r' = insert x r
        h  = height r'

        height :: Tree a -> Integer
        height Leaf = 0
        height (Node depth _ _ _) = depth

printTree :: (Show a) => Tree a -> String
printTree x = printTree' x 0
  where
    printTree' x depth = case x of
      Node d l v r -> (replicate (2 * depth) ' ')
                           ++ "D" ++ show d ++ " " ++ show v
                           ++ "\n"
                           ++ (printTree' l (depth + 1))
                           ++ "\n"
                           ++ (printTree' r (depth + 1))
      Leaf -> (replicate (2 * depth) ' ') ++ "Leaf"
