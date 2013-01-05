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
  , general
  -- * Kind Variables
  , HasKindVars(..)
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Ermine.Scope
import Ermine.Syntax
import Prelude.Extras
import Data.IntMap
import Data.Foldable
import Data.String
import Data.Traversable
import Data.Data
import Data.Map as Map

-- $setup
-- >>> :set -XOverloadedStrings

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

instance IsString a => IsString (Kind a) where
  fromString = Var . fromString

instance Fun Kind where
  fun = prism (uncurry (:->)) $ \t -> case t of
    l :-> r -> Right (l, r)
    _       -> Left t

instance Variable Kind where
  var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

instance Plated (Kind a) where
  plate f (l :-> r) = (:->) <$> f l <*> f r
  plate _ v = pure v

instance Functor Kind where
  fmap = fmapDefault

instance Foldable Kind where
  foldMap = foldMapDefault

instance Traversable Kind where
  traverse f (Var a)      = Var <$> f a
  traverse f (x :-> y)    = (:->) <$> traverse f x <*> traverse f y
  traverse _ (HardKind k) = pure $ HardKind k

instance Applicative Kind where
  pure = Var
  (<*>) = ap

instance Monad Kind where
  return = Var
  Var a >>= f      = f a
  (x :-> y) >>= f  = (x >>= f) :-> (y >>= f)
  HardKind k >>= _ = HardKind k

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

instance Fun Schema where
  fun = prism hither yon
    where
    hither (Schema n (Scope s), Schema m t) = Schema (n + m) $
      let Scope t' = mapBound (+n) t
      in Scope (s :-> t')
    yon t@(Schema n s) = case fromScope s of
      l :-> r -> case (maximumOf (traverse.bound) l, minimumOf (traverse.bound) r) of
        (Nothing, Nothing)              -> Right (Schema 0 (toScope l), Schema 0 (toScope r))
        (Nothing, Just 0)               -> Right (Schema 0 (toScope l), Schema n (toScope r))
        (Just m, Nothing)  | n == m + 1 -> Right (Schema n (toScope l), Schema 0 (toScope r))
        (Just m, Just o)   | m == o - 1 -> Right (Schema (m + 1) (toScope l), Schema (n - o) (toScope (r & mapped.bound -~ o)))
        _                               -> Left t
      _                                 -> Left t

instance Variable Schema where
  var = prism (schema . return) $ \ t@(Schema _ (Scope b)) -> case b of
    Var (F (Var k)) -> Right k
    _               -> Left  t

-- | Lift a kind into a kind schema
--
-- >>> schema (star ~> star)
-- Schema 0 (Scope (Var (F (HardKind Star :-> HardKind Star))))
schema :: Kind a -> Schema a
schema k = Schema 0 (lift k)

-- | Construct a schema from a kind, generalizing all free variables.
--
-- When working with 'Ermine.Kind.Inference.Meta' variables, you want to use
-- generalize to 'zonk' the kind and check skolems.
--
-- >>> general "a"
-- Schema 1 (Scope (Var (B 0)))
--
-- >>> general ("a" ~> "a")
-- Schema 1 (Scope (Var (B 0) :-> Var (B 0)))
--
-- >>> general ("a" ~> "b")
-- Schema 2 (Scope (Var (B 0) :-> Var (B 1)))
--
-- >>> general ("b" ~> "a")
-- Schema 2 (Scope (Var (B 0) :-> Var (B 1)))
--
-- >>> general (star ~> star)
-- Schema 0 (Scope (HardKind Star :-> HardKind Star))
general :: Ord k => Kind k -> Schema a
general k0 = Schema (snd mnl) (Scope r) where
 (mnl, r) = mapAccumL go (Map.empty, 0) k0
 go mn@(m, n) k = case m^.at k of
   Just b  -> (mn, B b)
   Nothing -> let n' = n + 1 in n' `seq` ((m & at k ?~ n, n'), B n)

instance Kindly (Schema a) where
  hardKind = prism (schema . review hardKind) $ \ t@(Schema _ (Scope b)) -> case b of
    HardKind k           -> Right k
    Var (F (HardKind k)) -> Right k
    _                    -> Left t

instance HasKindVars (Schema a) (Schema b) a b where
  kindVars = traverse

instance BoundBy Schema Kind where
  boundBy f (Schema i b) = Schema i (boundBy f b)
