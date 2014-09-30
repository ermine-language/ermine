module Pair where

fst : (a, b) -> a
fst (a, b) = a

snd : (a, b) -> b
snd (a, b) = b

mapFst : (a -> b) -> (a, c) -> (b, c)
mapFst f ~(a,b) = (f a, b)

mapSnd: (a -> b) -> (c, a) -> (c, b)
mapSnd f ~(a, b) = (a, f b)

