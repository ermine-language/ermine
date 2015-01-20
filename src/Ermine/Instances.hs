{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(a,b,c) 1
#endif

--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2015
-- License   :  BSD2
-- Maintainer:  Stephen Compall <scompall@nocandysw.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Orphan instances
--------------------------------------------------------------------

module Ermine.Instances () where

import Control.Monad.Trans
import Control.Monad.Trans.Except

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except()
#else
import Control.Monad.Error (MonadError(..))
import Control.Monad.State (MonadState(..))
#endif

-- Control.Monad.Except (containing the needed instance) was
-- introduced in mtl 2.2.1, which is incompatible with transformers <
-- 0.4
#if !(MIN_VERSION_mtl(2,2,1))
instance Monad m => MonadError e (ExceptT e m) where
  throwError = throwE
  catchError = catchE

instance MonadState s m => MonadState s (ExceptT e m) where
  state = lift . state
#endif
