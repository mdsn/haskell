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
