--a bad, not fast dictionary

module Qlib.Dict where

data Dict k v = Empty | Entry k v (Dict k v) deriving (Show)

instance Foldable (Dict k) where
    foldr _ b Empty = b
    foldr f b (Entry k a next) = foldr f (f a b) next

instance Functor (Dict k) where
    fmap _ Empty = Empty
    fmap f (Entry k v next) = Entry k (f v) (fmap f next)

instance Monoid (Dict k v) where
    mempty = Empty

instance Semigroup (Dict k v) where
    (<>) x Empty = x
    (<>) Empty y = y
    (<>) (Entry k v next) y = Entry k v $ next <> y

instance Traversable (Dict k) where
    traverse f Empty = pure Empty
    traverse f (Entry k v next) = Entry k <$> f v <*> traverse f next

elems :: Dict k v -> [v]
elems Empty = []
elems (Entry _ v next) = v : elems next

filterElems :: (v -> Bool) -> Dict k v -> Dict k v
filterElems _ Empty = Empty
filterElems f (Entry k v next)
    | f v = Entry k v (filterElems f next)
    | otherwise = filterElems f next

filterKeys :: (k -> Bool) -> Dict k v -> Dict k v
filterKeys _ Empty = Empty
filterKeys f (Entry k v next)
    | f k = Entry k v (filterKeys f next)
    | otherwise = filterKeys f next

fromList :: (Eq k) => [(k, v)] -> Dict k v
fromList [] = Empty
fromList (x:xs) = insert (fst x) (snd x) (fromList xs)

get :: (Eq k) => k -> Dict k v -> Maybe v
get _ Empty = Nothing
get k (Entry nk v next)
    | k == nk = Just v
    | otherwise = get k next

insert :: (Eq k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Entry k v Empty
insert k v (Entry nk nv next)
    | k == nk = Entry k v next
    | otherwise = Entry nk nv (insert k v next)

keys :: Dict k v -> [k]
keys Empty = []
keys (Entry k _ next) = k : keys next

singleton :: (Eq k) => k -> v -> Dict k v
singleton k v = Entry k v Empty

size :: Dict k v -> Int
size Empty = 0
size (Entry _ _ next) = 1 + size next

sortKeys :: (Ord k) => Dict k v -> Dict k v
sortKeys Empty = Empty
sortKeys (Entry k v next) = lesser <> greater
    where
        lesser = filterKeys (<=k) (Entry k v next)
        greater = filterKeys (>k) next

toList :: Dict k v -> [(k, v)]
toList Empty = []
toList (Entry k v next) = (k, v) : toList next

--left-preferring
union :: (Eq k) => Dict k v -> Dict k v -> Dict k v
union x Empty = x
union Empty y = y
union (Entry k v next) y = insert k v $ union next y

unions :: (Eq k, Foldable t) => t (Dict k v) -> Dict k v
unions = foldr union Empty

valid :: (Eq k) => Dict k v -> Bool
valid d = valid' d []
    where
        valid' :: (Eq k) => Dict k v -> [k] -> Bool
        valid' Empty _ = True
        valid' (Entry k _ next) ks
            | elem k ks = False
            | otherwise = valid' next (k:ks)
