module Qlib.BST where

data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

elem :: (Ord a) => a -> BST a -> Bool
elem _ Empty = False
elem x (Node n left right)
    | x == n = True
    | x < n = Qlib.BST.elem x left
    | otherwise = Qlib.BST.elem x right

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) Empty

insert :: (Ord a) => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node n left right)
    | x < n = Node n (insert x left) right
    | x > n = Node n left (insert x right)
    | otherwise = Node n left right

singleton :: (Ord a) => a -> BST a
singleton x = Node x Empty Empty

sortedList :: (Ord a) => BST a -> [a]
sortedList Empty = []
sortedList (Node n left right) = sortedList left ++ [n] ++ sortedList right

toList :: BST a -> [a]
toList Empty = []
toList (Node n left right) = [n] ++ toList left ++ toList right
