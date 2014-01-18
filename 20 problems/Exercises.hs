{-# LANGUAGE InstanceSigs #-}
-- 20 intermediate haskell projects
-- https://www.fpcomplete.com/user/DanBurton/20-intermediate-exercises

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry :: (a -> b) -> [a] -> [b]
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry :: (a -> b) -> Maybe a -> Maybe b
  furry f x = case x of
      Just a -> Just (f a)
      Nothing -> Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry :: (a -> b) -> (t -> a) -> (t -> b)
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry :: (a -> b) -> (EitherLeft t a) -> (EitherLeft t b)
  furry f (EitherLeft (Left a)) = EitherLeft $ Left (f a)
  furry f (EitherLeft (Right b)) = EitherLeft $ Right b

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry :: (a -> b) -> (EitherRight t a) -> (EitherRight t b)
  furry f (EitherRight (Left a)) = EitherRight $ Left a
  furry f (EitherRight (Right b)) = EitherRight . Right $ f b

main = putStrLn "It typechecks!"
