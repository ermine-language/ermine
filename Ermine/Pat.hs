{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Pat
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Pat
  ( Pat(..)
  , Alt(..)
  , bitraverseAlt
  ) where

import Bound
import Control.Applicative
import Data.Bitraversable
import Data.Foldable
import Data.Traversable
import Ermine.Prim
import Ermine.Scope
import Ermine.Mangled

-- | Patterns used by 'Term' and 'Core'.
data Pat t
  = VarP
  | SigP t             -- ^ not used by 'Core'
  | WildcardP
  | AsP (Pat t)
  | StrictP (Pat t)
  | LazyP (Pat t)
  | PrimP Prim [Pat t]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | One alternative of a core expression
data Alt t f a = Alt !(Pat t) !(Scope Int f a)
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance Bound (Alt t) where
  Alt p b >>>= f = Alt p (b >>>= f)

instance Monad f => BoundBy (Alt t f) f where
  boundBy f (Alt p b) = Alt p (boundBy f b)

bitraverseAlt :: (Bitraversable k, Applicative f) => (t -> f t') -> (a -> f b) -> Alt t (k t) a -> f (Alt t' (k t') b)
bitraverseAlt f g (Alt p b) = Alt <$> traverse f p <*> bitraverseScope f g b
