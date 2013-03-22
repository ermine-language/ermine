{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Syntax.Kind
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Kind
  (
  -- * Kinds
    Kind(..)
  , getKind
  , putKind
  -- * Hard Kinds
  , HardKind(..)
  , Kindly(..)
  -- * Kind Schemas
  , Schema(..)
  , schema
  , general
  , putSchema
  , getSchema
  -- * Kind Variables
  , HasKindVars(..)
  ) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Hashable.Extras
import Data.Foldable
import Data.IntMap
import Data.String
import Data.Traversable
import Data.Map as Map
import GHC.Generics
import Ermine.Syntax
import Ermine.Syntax.Scope
import Prelude.Extras

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
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable, Generic)

instance Hashable HardKind

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

instance Binary HardKind where
  put Star       = putWord8 0
  put Constraint = putWord8 1
  put Rho        = putWord8 2
  put Phi        = putWord8 3

  get = getWord8 >>= \b -> case b of
    0 -> return Star
    1 -> return Constraint
    2 -> return Rho
    3 -> return Phi
    _ -> fail $ "get HardKind: Unexpected constructor code: " ++ show b

------------------------------------------------------------------------------
-- Kind
------------------------------------------------------------------------------

-- | Kinds with kind variables of type @a@
data Kind a
  = Var a
  | Kind a :-> Kind a
  | HardKind HardKind
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Hashable a => Hashable (Kind a)
instance Hashable1 Kind

instance IsString a => IsString (Kind a) where
  fromString = Var . fromString

instance Fun (Kind a) where
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

instance Eq1 Kind
instance Ord1 Kind
instance Show1 Kind
instance Read1 Kind

putKind :: (k -> Put) -> Kind k -> Put
putKind p = go
 where
   go (Var v)      = putWord8 0 *> p v
   go (k :-> l)    = putWord8 1 *> go k *> go l
   go (HardKind k) = putWord8 2 *> put k
{-# INLINE putKind #-}

getKind :: Get k -> Get (Kind k)
getKind g = go
 where
   go = getWord8 >>= \b -> case b of
     0 -> Var <$> g
     1 -> (:->) <$> go <*> go
     2 -> HardKind <$> get
     _ -> fail $ "getKind: Unexpected constructor code: " ++ show b
{-# INLINE getKind #-}

instance Binary k => Binary (Kind k) where
  put = putKind put
  get = getKind get

------------------------------------------------------------------------------
-- HasKindVars
------------------------------------------------------------------------------

-- | Provides a traversal of free kind variables that can be used to perform substitution or extract a free variable set.
class HasKindVars s t a b | s -> a, s b -> t, t -> b, t a -> s where
  -- >>> ("b" :-> "a") ^.. kindVars
  -- ["b","a"]
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
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable, Generic)

instance Hashable a => Hashable (Schema a)
instance Hashable1 Schema

instance Fun (Schema a) where
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

putSchema :: (k -> Put) -> Schema k -> Put
putSchema pk (Schema n body) = put n *> putScope put putKind pk body
{-# INlINE putSchema #-}

getSchema :: Get k -> Get (Schema k)
getSchema gk = Schema <$> get <*> getScope get getKind gk
{-# INLINE getSchema #-}

instance Binary k => Binary (Schema k) where
  put = putSchema put
  get = getSchema get
