module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


instance Sized b => Sized (JoinList b a) where
    size Empty          = 0
    size (Single s _)   = size s
    size (Append s _ _) = size s


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ r     = r
l     +++ Empty = l
l     +++ r     = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single a _)   = a
tag (Append a _ _) = a


jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty         = Nothing
indexJ i _     | i < 0 = Nothing

indexJ i (Single s x)
  | i >= sz s       = Nothing
  | otherwise       = Just x
  where
    sz = getSize . size

indexJ i (Append s x y)
  | sz s <= i       = Nothing
  | (sz x - 1) >= i = indexJ i x
  | otherwise       = indexJ (i - sz x) y
  where
    sz :: Sized a => a -> Int
    sz = getSize . size


tree :: JoinList Size Char
tree = let a = single 'a'
           b = single 'b'
           c = a +++ b
           d = single 'd'
           e = c +++ d
           f = single 'f'
           g = single 'g'
           h = f +++ g
           i = e +++ h
           single = Single (Size 1)
        in i
