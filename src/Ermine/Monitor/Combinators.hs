{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ermine.Monitor.Combinators
  ( Setting(..)
  , Incremental(..)
  , dec
  , sub
  ) where

import Control.Monad.Trans

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  assign _ _ = return ()

  update :: MonadIO m => t -> (a -> a) -> m () -- modify
  update _ _ = return ()

dec :: (MonadIO m, Setting t a, Num a) => t -> m ()
dec t = update t (subtract 1)

sub :: (MonadIO m, Setting t a, Num a) => t -> a -> m ()
sub t n = update t (subtract n)

class Incremental t where
  inc :: MonadIO m => t -> m ()
  inc _ = return ()

  add :: MonadIO m => t -> Int -> m ()
  add _ _ = return ()
