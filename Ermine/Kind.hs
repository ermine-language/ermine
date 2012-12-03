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
  , Schema(..)
  , Kindly(..)
  , HasKindVars(..)
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Ermine.Mangled
import Prelude.Extras
import Data.IntMap
import Data.Foldable
import Data.Traversable
import Data.Data
import Data.Map

infixr 0 :->

data HardKind
  = Star
  | Constraint
  | Rho
  | Phi
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

class Kindly k where
  hardKind :: HardKind -> k
  star, constraint, rho, phi :: k
  star = hardKind Star
  constraint = hardKind Constraint
  rho = hardKind Rho
  phi = hardKind Phi

instance Kindly HardKind where
  hardKind = id

data Kind a
  = Var a
  | Kind a :-> Kind a
  | HardKind HardKind
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Plated (Kind a) where
  plate f (l :-> r) = (:->) <$> f l <*> f r
  plate _ v = pure v

instance Mangled Kind where
  mangled f (Var a)      = f a
  mangled f (x :-> y)    = (:->) <$> mangled f x <*> mangled f y
  mangled _ (HardKind k) = pure $ HardKind k

instance Functor Kind where
  fmap = fmapDefault

instance Foldable Kind where
  foldMap = foldMapDefault

instance Traversable Kind where
  traverse = traverseDefault

instance Applicative Kind where
  pure = Var
  (<*>) = ap

instance Monad Kind where
  return = Var
  (>>=) = bindDefault

instance Kindly (Kind a) where
  hardKind = HardKind

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

instance HasKindVars s t a b => HasKindVars (Map k s) (Map k t) a b where
  kindVars = traverse.kindVars

-- | Kind schemas
data Schema a = Schema !Int !(Scope Int Kind a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)

kindSchema :: Kind a -> Schema a
kindSchema k = Schema 0 (lift k)

instance Kindly (Schema a) where
  hardKind = kindSchema . hardKind

instance HasKindVars (Schema a) (Schema b) a b where
  kindVars = traverse

instance BoundBy Schema Kind where
  boundBy f (Schema i b) = Schema i (boundBy f b)

instance MangledBy Schema Kind where
  mangledBy f (Schema i b) = Schema i <$> mangledBy f b
