module Type.Cast where

import Function
import Maybe
import Syntax.Maybe

infix type 4 <:

data (<:) a b = Cast (a -> b) (b -> Maybe a)

upcast : a <: b -> a -> b
upcast (Cast e _) = e

downcast : a <: b -> b -> Maybe a
downcast (Cast _ p) = p

refl : a <: a
refl = Cast id Just

trans : b <: c -> a <: b -> a <: c
trans (Cast e' p') (Cast e p) = Cast (e' . e) (c -> p' c >>= p)
