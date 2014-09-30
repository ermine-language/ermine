module Control.Monad.Reader where

import Control.Monad
import Prelude hiding {bind; unit}

type Reader e a = e -> a
runReader : Reader e a -> e -> a
runReader = id

readerFunctor : Functor ((->) e)
readerFunctor = Functor (.)

readerAp : Ap ((->) e)
readerAp = Ap const ap'
private
  ap' : Reader e  (a -> b) -> Reader e a -> Reader e b
  ap' f g e = f e $ g e

readerMonad : Monad ((->) e)
readerMonad = Monad (a -> e -> a) bind'
private
  bind' r k e = k (r e) e

ask : Reader a a
ask = id

local: (e -> e) -> Reader e a -> Reader e a
local = flip (.)

data ReaderT r m a = ReaderT (r -> m a)
runReaderT : ReaderT r m a -> r -> m a
runReaderT (ReaderT f) = f

readerTMonad : Monad m -> Monad (ReaderT r m)
readerTMonad ad = Monad return' bond' where
  return' a = ReaderT (r -> (unit ad a))
  bond' m k = ReaderT (r -> bind ad (runReaderT m r) (a -> runReaderT (k a) r))

askT: Monad m -> ReaderT r m r
askT = ReaderT . unit

localT: (e -> e) -> ReaderT e m a -> ReaderT e m a
localT f r = ReaderT (runReaderT r . f)
