intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (y:[]) = [y]
intersperse x (y:ys) = y:x:intersperse x ys 

intercalate :: [a] -> [[a]] -> [a]
intercalate x (y:[]) = y
intercalate x (y:ys) = y ++ x ++ intercalate x ys
