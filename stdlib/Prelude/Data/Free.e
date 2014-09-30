module Data.Free where

import Control.Monad

data Free f a
  = Pure a
  | Free (f (Free f a))

private
  freeMap _  f (Pure a) = Pure (f a)
  freeMap ff f (Free as) = Free (fmap ff (freeMap ff f) as)

  freeBind _  (Pure a) f = f a
  freeBind ff (Free as) f = Free (fmap ff (m -> freeBind ff m f) as)

freeFunctor f = Functor (freeMap f)
freeMonad f = Monad Pure (freeBind f)

liftFree : Functor f -> f a -> Free f a
liftFree ff fa = Free (fmap ff Pure fa)

lowerFree : Monad m -> Free m a -> m a
lowerFree mf (Pure a) = unit mf a
lowerFree mf (Free as) = bind mf as (lowerFree mf)
