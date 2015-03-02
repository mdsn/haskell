{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified ExprT as E
import StackVM
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr Program where
    lit n   = [PushI n]
    mul x y = x ++ y ++ [Mul]
    add x y = x ++ y ++ [Add]

instance Expr E.ExprT where
    lit = E.Lit
    mul = E.Mul
    add = E.Add

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

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    mul (Mod7 x) (Mod7 y) = lit $ x * y
    add (Mod7 x) (Mod7 y) = lit $ x + y

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

reify :: E.ExprT -> E.ExprT
reify = id

eval :: E.ExprT -> Integer
eval e       = case e of
    (E.Add x y) -> recurse (+) x y
    (E.Mul x y) -> recurse (*) x y
    (E.Lit n)   -> n
  where
    recurse :: (Integer -> Integer -> Integer) -> E.ExprT -> E.ExprT -> Integer
    recurse f x y = f (eval x) (eval y)

evalStr :: String -> Maybe Integer
evalStr s =
    case parseExp E.Lit E.Add E.Mul s of
        Just e -> Just $ eval e
        _      -> Nothing

