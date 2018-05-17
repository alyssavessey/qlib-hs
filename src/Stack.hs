module Stack where

import Data.Maybe (fromJust)

data Stack a = Bottom | Value a (Stack a) deriving (Eq, Show)

instance Applicative (Stack) where
    pure a = Value a Bottom
    Bottom <*> _ = Bottom
    _ <*> Bottom = Bottom
    (Value f x) <*> (Value g y) = Value (f g) (x <*> y)

instance Foldable (Stack) where
    foldr _ b Bottom = b
    foldr f b (Value a s) = foldr f (f a b) s

instance Functor (Stack) where
    fmap _ Bottom = Bottom
    fmap f (Value a s) = Value (f a) (fmap f s)

instance Semigroup (Stack s) where
    (<>) Bottom s = s
    (<>) s Bottom = s
    (<>) (Value a x) y = Value a $ x <> y

instance Monad (Stack) where
    Bottom >>= _ = Bottom
    (Value a s) >>= f = if Stack.null fa then Bottom else Value (fromJust $ top $ fa) (s >>= f)
        where
            fa = f a

instance Monoid (Stack s) where
    mempty = Bottom

instance Traversable (Stack) where
    traverse _ Bottom = pure Bottom
    traverse f (Value a s) = Value <$> f a <*> traverse f s

fromList :: [a] -> Stack a
fromList [] = Bottom
fromList (x:xs) = push x $ fromList xs

null :: Stack a -> Bool
null Bottom = True
null _ = False

pop :: Stack a -> Maybe (Stack a, a)
pop Bottom = Nothing
pop (Value a s) = Just (s, a)

push :: a -> Stack a -> Stack a
push a s = Value a s

singleton :: a -> Stack a
singleton a = Value a Bottom

size :: Stack a -> Int
size = foldr (\_ b -> b + 1) 0

top :: Stack a -> Maybe a
top Bottom = Nothing
top (Value a _) = Just a

toList :: Stack a -> [a]
toList Bottom = []
toList (Value a s) = a : toList s
