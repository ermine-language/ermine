{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module provides extensions to the @bound@ package.
--------------------------------------------------------------------
module Ermine.Syntax.Scope
  ( hoistScope
  , bitraverseScope
  , transverseScope
  , BoundBy(..)
  , instantiateVars
  -- , putVar , getVar , putScope, getScope
  , serializeScope3
  , deserializeScope3
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put

-- | Generalizes 'Bound' to permit binding by another type without taking it as a parameter.
class Monad m => BoundBy tm m | tm -> m where
  boundBy :: (a -> m b) -> tm a -> tm b

instance Monad m => BoundBy (Scope b m) m where
  boundBy = flip (>>>=)
  {-# INLINE boundBy #-}

serializeScope3 :: MonadPut m
                => (b -> m ()) -> (forall a. (a -> m ()) -> f a -> m ()) -> (v -> m ())
                -> Scope b f v -> m ()
serializeScope3 pb pf pv (Scope e) = pf (serializeWith2 pb (pf pv)) e

deserializeScope3 :: MonadGet m
                  => m b -> (forall a. m a -> m (f a)) -> m v
                  -> m (Scope b f v)
deserializeScope3 gb gf gv = Scope <$> gf (deserializeWith2 gb (gf gv))
