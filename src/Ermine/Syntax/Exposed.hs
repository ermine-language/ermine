{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Syntax.Exposed
  ( Exposed(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Foldable
import Data.Functor.Extend
import Data.Hashable
import Data.Hashable.Extras
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Typeable
import Data.Void
import Ermine.Syntax.Type
import GHC.Generics

data Exposed a = Exposed { _exposedType :: !(Type Void Void), _exposedValue :: a }
  deriving (Eq,Show,Functor, Foldable, Traversable,Typeable,Generic)

makeClassy ''Exposed

instance Hashable a => Hashable (Exposed a)

instance Hashable1 Exposed

instance Foldable1 Exposed where
  foldMap1 f (Exposed _ a) = f a

instance Traversable1 Exposed where
  traverse1 f (Exposed n a) = Exposed n <$> f a

instance Comonad Exposed where
  extract (Exposed _ a) = a
  extend f w@(Exposed t _) = Exposed t (f w)

instance Extend Exposed where
  extended f w@(Exposed t _) = Exposed t (f w)
