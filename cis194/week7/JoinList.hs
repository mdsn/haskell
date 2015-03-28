{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
  #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Sized
import Scrabble

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


sz :: Sized a => a -> Int
sz = getSize . size


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty         = Nothing
indexJ i _     | i < 0 = Nothing

indexJ i (Single s x)
  | i >= sz s       = Nothing
  | otherwise       = Just x

indexJ i (Append s x y)
  | sz s <= i       = Nothing
  | (sz x - 1) >= i = indexJ i x
  | otherwise       = indexJ (i - sz x) y


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty          = Empty
dropJ n x | n <= 0     = x
dropJ _ (Single _ _)   = Empty
dropJ n (Append s x y)
  | sz s == n = Empty
  | sz x >= n = dropJ n x +++ y
  | otherwise = dropJ (n - sz x) y


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ n _ | n <= 0     = Empty
takeJ _ t@(Single _ _) = t
takeJ n t@(Append s x y)
  | sz s == n = t
  | sz x >= n = takeJ n x
  | otherwise = x +++ takeJ (n - sz x) y

scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine xs = Single (scoreString xs) xs

single :: String -> JoinList (Score, Size) String
single s = Single (scoreString s, 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ x)   = x
  toString (Append _ x y) = toString x ++ toString y

  fromString [] = Empty
  fromString s = foldr (\x b -> single x +++ b) Empty xs
    where
      xs = lines s

  replaceLine n _ b | n < 0                 = b
  replaceLine n _ b | n >= getSize (size b) = b
  replaceLine n s b = l +++ fromString s +++ r
    where
      l = takeJ n b
      r = dropJ (n + 1) b

  line = indexJ
  numLines = getSize . size

  value Empty          = 0
  value (Single s _)   = getScore $ fst s
  value (Append s _ _) = getScore $ fst s

main :: IO ()
main = runEditor editor buf
 
buf :: JoinList (Score, Size) String
buf = a +++ b +++ c +++ d
  where
    a = fromString "Editor is an instance of Monad but not of Applicative - "
    b = fromString "this will become an error in GHC 7.10, "
    c = fromString "under the Applicative-Monad Proposal. "
    d = fromString "-Sincerely, GHC"
