module Control.Monad.Cont where

import Control.Monad
import Error
import Function

data Cont r a = Cont ((a -> r) -> r)

runCont : Cont r a -> ((a -> r) -> r)
runCont (Cont f) = f

mapCont : (r -> r) -> Cont r a -> Cont r a
mapCont f m = Cont (f . runCont m)

withCont : ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f m = Cont (runCont m . f)

contMonad : Monad (Cont r)
contMonad = Monad unit' bind'
private
  unit' a = Cont (c -> c a)
  bind' m k = Cont (c -> runCont m (a -> runCont (k a) c))

contAp = monadAp contMonad
contFunctor = monadFunctor contMonad
