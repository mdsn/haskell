data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving Show

plant :: Tree Int
plant = Branch (Branch (Leaf 3)
                       (Branch (Leaf 2)
                               (Leaf 1)))
               (Branch (Leaf 4)
                       (Leaf 3))
 
-- length of the longest path from root to leaf
height :: Tree a -> Int
height = (flip g) 0
    where 
        g (Leaf _) i     = i + 1
        g (Branch a b) i = let j = i + 1 in max (g a j) (g b j)

-- count leaves
leaves :: Tree a -> Int
leaves = (flip g) 0
    where
        g (Leaf _) i = i + 1
        g (Branch a b) i = (g a i) + (g b i)

-- count non leaf nodes
nodes :: Tree a -> Int
nodes (Leaf _) = 0
nodes (Branch a b) = 1 + (nodes a) + (nodes b)

-- list leaves left to right
walkover :: Tree a -> [Tree a]
walkover = (flip g) []
    where
        g (Leaf a) leaves = leaves ++ [Leaf a]
        g (Branch a b) leaves = (g a leaves) ++ (g b [])
