module Calc where

import ExprT
import Parser

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