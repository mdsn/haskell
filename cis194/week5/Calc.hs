module Calc where

import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit n | n <= 0    = False
          | otherwise = True
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit = MinMax
    mul = max
    add = min

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval e       = case e of
    (Add x y) -> recurse (+) x y
    (Mul x y) -> recurse (*) x y
    (Lit n)   -> n
  where
    recurse :: (Integer -> Integer -> Integer) -> ExprT -> ExprT -> Integer
    recurse f x y = f (eval x) (eval y)

evalStr :: String -> Maybe Integer
evalStr s =
    case parseExp Lit Add Mul s of
        Just e -> Just $ eval e
        _      -> Nothing

