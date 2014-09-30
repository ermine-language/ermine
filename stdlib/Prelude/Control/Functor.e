module Control.Functor where

type FunctorMap f = forall a b. (a -> b) -> f a -> f b
data Functor f = Functor (forall a b. (a -> b) -> f a -> f b)

fmap : Functor f -> (a -> b) -> f a -> f b
fmap (Functor map) f xs = map f xs

strength : Functor f -> (x, f y) -> f (x, y)
strength f (x, fy) = fmap f (y -> (x, y)) fy

-- | Specialization of cosequence.
mapply : Functor f -> f (a -> b) -> a -> f b
mapply ev xs a = fmap ev (f -> f a) xs
