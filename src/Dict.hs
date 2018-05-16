module Qlib.Dict where

import Data.Maybe

data Dict k v = Empty | Entry k v (Dict k v) deriving (Show)

instance Functor (Dict k) where
    fmap _ Empty = Empty
    fmap f (Entry k v next) = Entry k (f v) (fmap f next)

insert :: (Eq k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Entry k v Empty
insert k v (Entry nk nv next)
    | k == nk = Entry k v next
    | otherwise = Entry nk nv (insert k v next)

get :: (Eq k) => k -> Dict k v -> Maybe v
get _ Empty = Nothing
get k (Entry nk v next)
    | k == nk = Just v
    | otherwise = get k next

singleton :: (Eq k) => k -> v -> Dict k v
singleton k v = Entry k v Empty

toList :: Dict k v -> [(k, v)]
toList Empty = []
toList (Entry k v next) = (k, v) : toList next
