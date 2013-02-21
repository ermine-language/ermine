{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Unification.Sharing
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- These combinators can be used to retain sharing information.
--------------------------------------------------------------------
module Ermine.Unification.Sharing
  ( runSharing
  , sharing
  , SharingT(..)
  , Shared(..)
  ) where

import Control.Applicative
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import Data.Traversable

data Shared a = Shared !Bool a
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

-- An efficient strict-in-the-monoid version of WriterT Any@
newtype SharingT m a = SharingT { unsharingT :: m (Shared a) }

instance Monad m => Functor (SharingT m) where
  fmap f (SharingT m) = SharingT $ do
    Shared p a <- m
    return $! Shared p (f a)
  {-# INLINE fmap #-}

instance Monad m => Applicative (SharingT m) where
  pure a = SharingT (return (Shared False a))
  {-# INLINE pure #-}
  SharingT mf <*> SharingT ma = SharingT $ do
    Shared p f <- mf
    Shared q a <- ma
    return $! Shared (p || q) (f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (SharingT m) where
  return a = SharingT (return (Shared False a))
  {-# INLINE return #-}
  SharingT m >>= f = SharingT $ do
    Shared p a <- m
    Shared q b <- unsharingT (f a)
    return $! Shared (p || q) b
  {-# INLINE (>>=) #-}

instance Monad m => MonadWriter Any (SharingT m) where
  tell (Any p) = SharingT $ return $ Shared p ()
  {-# INLINE tell #-}
  listen (SharingT ma) = SharingT $ do
    Shared p a <- ma
    return $! Shared p (a, Any p)
  {-# INLINE listen #-}
  pass (SharingT mapp) = SharingT $ do
    Shared p (a, pp) <- mapp
    return $! Shared (getAny (pp (Any p))) a
  {-# INLINE pass #-}

instance MonadTrans SharingT where
  lift ma = SharingT $ do
    a <- ma
    return $! Shared False a
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (SharingT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (SharingT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (SharingT m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = SharingT . local f . unsharingT
  {-# INLINE local #-}

-- | Run an action, if it returns @'Any' 'True'@ then use its new value, otherwise use the passed in value.
--
-- This can be used to recover sharing during unification when no interesting unification takes place.
--
-- This version discards the 'SharingT' wrapper.
runSharing :: Monad m => a -> SharingT m a -> m a
runSharing a m = do
  Shared modified b <- unsharingT m
  return $! if modified then b else a
{-# INLINE runSharing #-}

-- | Run an action, if it returns @'Any' 'True'@ then use its new value, otherwise use the passed in value.
--
-- This can be used to recover sharing during unification when no interesting unification takes place.
--
-- This version retains the current monad wrapper.
sharing :: MonadWriter Any m => a -> m a -> m a
sharing a m = do
  (b, Any modified) <- listen m
  return $! if modified then b else a
{-# INLINE sharing #-}
