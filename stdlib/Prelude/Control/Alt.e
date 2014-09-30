module Control.Alt where

import Control.Monoid
import Builtin as Builtin

export Control.Ap

data Alt f = Alt (forall a. f a) (forall a. f a -> f a -> f a) (Ap f)

empty : Alt f -> f a
empty (Alt z p t) = z

alt : Alt f -> f a -> f a -> f a
alt (Alt z p t) = p

altAp : Alt f -> Ap f
altAp (Alt z p t) = t

altMonoid : Alt f -> Monoid (f a)
altMonoid f = Monoid (empty f) (alt f)
