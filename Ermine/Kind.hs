{-# LANGUAGE TypeFamilies #-}
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
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Kind
  (
  -- * Kinds
    Kind(..)
  -- * Hard Kinds
  , HardKind(..)
  , Kindly(..)
  -- * Kind Schemas
  , Schema(..)
  , schema
  -- * Kind Variables
  , HasKindVars(..)
  , HasKind(..)
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Ermine.Mangled
import Ermine.Variable
import Prelude.Extras
import Data.IntMap
import Data.Foldable
import Data.Traversable
import Data.Data
import Data.Map

infixr 0 :->

------------------------------------------------------------------------------
-- HardKind
------------------------------------------------------------------------------

-- | Kinds that can be compared by simple structural equality
data HardKind
  = Star
  | Constraint
  | Rho
  | Phi
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable)

------------------------------------------------------------------------------
-- Kindly
------------------------------------------------------------------------------

-- | Smart constructor for hard kinds
class Kindly k where
  hardKind :: Prism' k HardKind
  star, constraint, rho, phi :: k
  star       = review hardKind Star
  constraint = review hardKind Constraint
  rho        = review hardKind Rho
  phi        = review hardKind Phi

instance Kindly HardKind where
  hardKind = id

------------------------------------------------------------------------------
-- Kind
------------------------------------------------------------------------------

-- | Kinds with kind variables of type @a@
data Kind a
  = Var a
  | Kind a :-> Kind a
  | HardKind HardKind
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Variable Kind where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

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
  hardKind = prism HardKind $ \t -> case t of
    HardKind k -> Right k
    _          -> Left t

instance Eq1 Kind where (==#) = (==)
instance Ord1 Kind where compare1 = compare
instance Show1 Kind where showsPrec1 = showsPrec
instance Read1 Kind where readsPrec1 = readsPrec

------------------------------------------------------------------------------
-- HasKindVars
------------------------------------------------------------------------------

-- | Provides a traversal of free kind variables that can be used to perform substitution or extract a free variable set.
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

------------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------------

-- | Kind schemas
data Schema a = Schema !Int !(Scope Int Kind a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)

instance Variable Schema where
  var = prism (schema . return) $ \ t@(Schema _ (Scope b)) -> case b of
    Var (F (Var k)) -> Right k
    _               -> Left  t

-- | Lift a kind into a kind schema
schema :: Kind a -> Schema a
schema k = Schema 0 (lift k)

instance Kindly (Schema a) where
  hardKind = prism (schema . review hardKind) $ \ t@(Schema _ (Scope b)) -> case b of
    HardKind k           -> Right k
    Var (F (HardKind k)) -> Right k
    _                    -> Left t

instance HasKindVars (Schema a) (Schema b) a b where
  kindVars = traverse

instance BoundBy Schema Kind where
  boundBy f (Schema i b) = Schema i (boundBy f b)

instance MangledBy Schema Kind where
  mangledBy f (Schema i b) = Schema i <$> mangledBy f b

------------------------------------------------------------------------------
-- HasKind
------------------------------------------------------------------------------

class HasKind s a | s -> a where
  kind :: Lens' s (Kind a)

instance HasKind (Kind a) a where
  kind = id
