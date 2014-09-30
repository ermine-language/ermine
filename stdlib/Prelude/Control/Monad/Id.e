module Control.Monad.Id where

import Control.Monad

data Id a = Id a
runIdentity : Id a -> a
runIdentity (Id a) = a

idFunctor : Functor Id
idFunctor = Functor map'
private
  map' : (a -> b) -> Id a -> Id b
  map' f (Id a) = Id (f a)

idAp : Ap Id
idAp = Ap Id ap'
private
  ap' : Id (a -> b) -> Id a -> Id b
  ap' (Id f) (Id a) = Id (f a)

idMonad : Monad Id
idMonad = Monad Id bind'
private
  bind' : Id a -> (a -> Id b) -> Id b
  bind' (Id a) f = f a
