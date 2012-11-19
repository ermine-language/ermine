{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Kind
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--------------------------------------------------------------------
module Ermine.Kind
  ( HardKind(..)
  , Kind(..)
  , KindSchema(..)
  , Kindly(..)
  , HasKindVars(..)
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Class
import Prelude.Extras
import Data.IntMap
import Data.Foldable
import Data.Data
import Data.Map

data HardKind
  = Star
  | Constraint
  | Rho
  | Phi
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

class Kindly k where
  star, constraint, rho, phi :: k

instance Kindly HardKind where
  star = Star
  constraint = Constraint
  rho = Rho
  phi = Phi

data Kind a
  = VarK a
  | ArrK (Kind a) (Kind a)
  | HardKind HardKind
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Data, Typeable)

instance Plated (Kind a) where
  plate f (ArrK l r) = ArrK <$> f l <*> f r
  plate _ v = pure v

instance Monad Kind where
  return = VarK
  VarK a     >>= f = f a
  ArrK ka kb >>= f = ArrK (ka >>= f) (kb >>= f)
  HardKind k    >>= _ = HardKind k

instance Kindly (Kind a) where
  star = HardKind Star
  constraint = HardKind Constraint
  rho = HardKind Rho
  phi = HardKind Phi

instance Eq1 Kind where (==#) = (==)
instance Ord1 Kind where compare1 = compare
instance Show1 Kind where showsPrec1 = showsPrec
instance Read1 Kind where readsPrec1 = readsPrec

class HasKindVars s t a b | s -> a, s b -> t, t -> b, t a -> s where
  kindVars :: Traversal s t a b

instance HasKindVars (Kind a) (Kind b) a b where
  kindVars = traverse

instance HasKindVars s t a b => HasKindVars [s] [t] a b where
  kindVars = traverse.kindVars

instance HasKindVars s t a b => HasKindVars (IntMap s) (IntMap t) a b where
  kindVars = traverse.kindVars

instance (Ord k, HasKindVars s t a b) => HasKindVars (Map k s) (Map k t) a b where
  kindVars = traverse.kindVars

data KindSchema a = ForallK !Int !(Scope Int Kind a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)

kindSchema :: Kind a -> KindSchema a
kindSchema k = ForallK 0 (lift k)

instance Kindly (KindSchema a) where
  star = kindSchema star
  constraint = kindSchema constraint
  rho = kindSchema rho
  phi = kindSchema phi

instance HasKindVars (KindSchema a) (KindSchema b) a b where
  kindVars = traverse

