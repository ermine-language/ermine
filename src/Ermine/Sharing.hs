{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Sharing
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Sharing
  ( runSharing
  , sharing
  ) where

import Control.Monad.Writer.Strict

-- | Run an action, if it returns @Any True@ then use its new value, otherwise use the passed in value.
--
-- This can be used to recover sharing during unification when no interesting unification takes place.
runSharing :: Monad m => a -> WriterT Any m a -> m a
runSharing a m = do
  (b, Any modified) <- runWriterT m
  return $ if modified then b else a
{-# INLINE runSharing #-}

sharing :: MonadWriter Any m => a -> m a -> m a
sharing a m = do
  (b, Any modified) <- listen m
  return $ if modified then b else a
{-# INLINE sharing #-}

