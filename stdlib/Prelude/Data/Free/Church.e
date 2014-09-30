module Data.Free.Church where

-- http://comonad.com/reader/2011/free-monads-for-less-2/
--
-- this representation fuses together binds, but also
-- has the benefit of not requiring a functor 'instance' for f
-- so we can map, etc. without tedious plumbing

import Control.Monad
import Function

infixl 4 >>= <*>

data F f a = F (forall r. (a -> r) -> (f r -> r) -> r)

fFunctor = Functor map
fAp = Ap return (<*>)
fMonad = Monad return (>>=)

runF : F f a -> (a -> r) -> (f r -> r) -> r
runF (F m) = m

private
  map f (F g) = F (kp -> g (kp . f))
  return a = F (kp _ -> kp a)
  (>>=) m f = F (kp kf -> runF m (a -> runF (f a) kp kf) kf)
  (<*>) mf ma = mf >>= f -> map f ma
