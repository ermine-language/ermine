module Control.Comonad where

import Function
export Control.Functor

type Extract f = forall a. f a -> a
type Extend f = forall a b. (f a -> b) -> f a -> f b

data Comonad f = Comonad (Extract f) (Extend f)

extract : Comonad f -> f a -> a
extract (Comonad (e : some f. Extract f) (d: some f. Extend f)) = e

extend : Comonad f -> (f a -> b) -> f a -> f b
extend (Comonad (e : some f. Extract f) (d: some f. Extend f)) = d

liftW : Comonad f -> (a -> b) -> f a -> f b
liftW w f = extend w (f . extract w)

comonadFunctor : Comonad f -> Functor f
comonadFunctor w = Functor (liftW w)
