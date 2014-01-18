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

-- Misty (5 to 10)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana :: (a -> [b]) -> [a] -> [b]  -- concatMap
  banana f = concat . map f

  unicorn :: a -> [a]
  unicorn = (:[])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana :: (a -> Maybe b) -> Maybe a -> Maybe b
  banana f x = case x of Just a -> f a
                         Nothing -> Nothing
  unicorn :: a -> Maybe a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- ((->) t) = (t ->)
  -- let m = (t ->)
  --         a ->  m    b   ->  m    a  ->  m    b
  banana :: (a -> (t -> b)) -> (t -> a) -> (t -> b)
  banana f g = \t -> f (g t) t

  unicorn :: a -> (t -> a)  -- const
  unicorn = \a _ -> a

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana :: (a -> EitherLeft t b) -> (EitherLeft t a) -> (EitherLeft t b)
  banana f (EitherLeft x) = case x of Left a -> f a
                                      Right b -> EitherLeft $ Right b

  unicorn :: a -> (EitherLeft t a)
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana :: (a -> EitherRight t b) -> (EitherRight t a) -> (EitherRight t b)
  banana f (EitherRight x) = case x of Left t -> EitherRight $ Left t
                                       Right a -> f a

  unicorn :: a -> (EitherRight t a)
  unicorn = EitherRight . Right

-- Misty (10 to 15)

-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f = banana (unicorn . f)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x f = banana (\g -> furry' g x) f

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = (sequence' . map f) xs
  where
    sequence' :: (Misty m) => [m a] -> m [a]
    sequence' as = foldr k (unicorn []) as

    k m m' = banana (\x ->
            (banana (\xs -> unicorn (x:xs)) m')) m

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = (flip moppy) id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f a b = b `apple` (furry' f a)

banana2' :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2' f a b = b `apple` (a `apple` (unicorn f))

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

main = putStrLn "It typechecks!"
