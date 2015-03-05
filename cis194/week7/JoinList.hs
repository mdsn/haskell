module JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ r     = r
l     +++ Empty = l
l     +++ r     = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single a _)   = a
tag (Append a _ _) = a
