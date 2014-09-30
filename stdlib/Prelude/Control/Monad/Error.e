module Control.Monad.Error where

import Control.Monad
import Either

data ErrorT e m a = ErrorT (m (Either e a))
runErrorT : ErrorT e m a -> m (Either e a)
runErrorT (ErrorT m) = m

errorTMonad : Monad m -> Monad (ErrorT e m)
errorTMonad ad = Monad return' bind' where
  return' a = ErrorT (unit ad (Right a))
  -- bind' : ErrorT e m a -> (a -> ErrorT e m b) -> ErrorT e m b
  bind' m k = ErrorT (bind ad (runErrorT m) (a -> case a of
    Left l -> unit ad (Left l)
    Right r -> runErrorT (k r)
  ))

fail: Monad m -> e -> ErrorT e m a
fail m e = ErrorT (unit m (Left e))

success: Monad m1 -> a1 -> ErrorT e1 m1 a1
success m1 a1 = ErrorT (unit m1 (Right a1))
