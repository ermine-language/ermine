{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
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
  -- * Hard Kinds
  , HardKind(..)
  , Kindly(..)
  -- * Kind Schemas
  , Schema(..)
  , schema
  , general
  -- * Kind Variables
  , HasKindVars(..)
  -- * Well formed kinds and boxities
  , wfk, wfb
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Serial
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Data
import Data.Hashable
import Data.Hashable.Extras
import Data.Foldable
import Data.IntMap
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.String
import Data.Traversable
import Data.Map as Map
import GHC.Generics
import Ermine.Syntax
import Ermine.Syntax.Digest
import Ermine.Syntax.Hint
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
  | Unboxed
  | Native
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Data, Typeable, Generic)

instance Hashable HardKind
instance Serial HardKind
instance Digestable HardKind

------------------------------------------------------------------------------
-- Kindly
------------------------------------------------------------------------------

-- | Smart constructor for hard kinds
class Kindly k where
  hardKind :: Prism' k HardKind
  star, constraint, rho, phi, unboxed, native :: k
  star       = review hardKind Star
  constraint = review hardKind Constraint
  rho        = review hardKind Rho
  phi        = review hardKind Phi
  unboxed    = review hardKind Unboxed
  native     = review hardKind Native

instance Kindly HardKind where
  hardKind = id

instance Binary HardKind where
  put Star       = putWord8 0
  put Constraint = putWord8 1
  put Rho        = putWord8 2
  put Phi        = putWord8 3
  put Unboxed    = putWord8 4
  put Native     = putWord8 5

  get = getWord8 >>= \b -> case b of
    0 -> return Star
    1 -> return Constraint
    2 -> return Rho
    3 -> return Phi
    4 -> return Unboxed
    5 -> return Native
    _ -> fail $ "get HardKind: Unexpected constructor code: " ++ show b

------------------------------------------------------------------------------
-- Kind
------------------------------------------------------------------------------

-- | Kinds with kind variables of type @a@
data Kind a
  = Var a
  | Kind a :-> Kind a
  | Type (Kind a)
  | HardKind HardKind
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Hashable a => Hashable (Kind a)
instance Hashable1 Kind
instance Digestable k => Digestable (Kind k)
instance Digestable1 Kind

instance IsString a => IsString (Kind a) where
  fromString = Var . fromString

instance Fun Kind where
  _Fun = prism (uncurry (:->)) $ \t -> case t of
    l :-> r -> Right (l, r)
    _       -> Left t

instance Variable Kind where
  _Var = prism Var $ \t -> case t of
    Var a -> Right a
    _     -> Left  t

instance Plated (Kind a) where
  plate f (l :-> r) = (:->) <$> f l <*> f r
  plate f (Type k) = Type <$> f k
  plate _ v = pure v

instance Functor Kind where
  fmap = fmapDefault

instance Foldable Kind where
  foldMap = foldMapDefault

instance Traversable Kind where
  traverse f (Var a)      = Var <$> f a
  traverse f (x :-> y)    = (:->) <$> traverse f x <*> traverse f y
  traverse f (Type k)     = Type <$> traverse f k
  traverse _ (HardKind k) = pure $ HardKind k

instance Applicative Kind where
  pure = Var
  (<*>) = ap

instance Monad Kind where
  return = Var
  Var a >>= f      = f a
  (x :-> y) >>= f  = (x >>= f) :-> (y >>= f)
  Type k >>= f = Type (k >>= f)
  HardKind k >>= _ = HardKind k

instance Kindly (Kind a) where
  hardKind = prism HardKind $ \t -> case t of
    HardKind k -> Right k
    _          -> Left t
  star    = Type (hardKind # star)
  unboxed = Type (hardKind # unboxed)
  native  = Type (hardKind # native)

instance Eq1 Kind
instance Ord1 Kind
instance Show1 Kind
instance Read1 Kind

instance Serial1 Kind where
  serializeWith p = go where
    go (Var v)      = putWord8 0 >> p v
    go (k :-> l)    = putWord8 1 >> go k >> go l
    go (Type k)     = putWord8 2 >> go k
    go (HardKind k) = putWord8 3 >> serialize k
  {-# INLINE serializeWith #-}

  deserializeWith g = go where
    go = getWord8 >>= \b -> case b of
      0 -> liftM Var g
      1 -> liftM2 (:->) go go
      2 -> liftM Type go
      3 -> liftM HardKind deserialize
      _ -> fail $ "getKind: Unexpected constructor code: " ++ show b
  {-# INLINE deserializeWith #-}

instance Serial a => Serial (Kind a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Binary k => Binary (Kind k) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize k => Serialize (Kind k) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

------------------------------------------------------------------------------
-- HasKindVars
------------------------------------------------------------------------------

-- | Provides a traversal of free kind variables that can be used to perform substitution or extract a free variable set.
class HasKindVars s t a b | s -> a, s b -> t, t -> b, t a -> s where
  -- >>> ("b" :-> "a") ^.. kindVars
  -- ["b","a"]
  kindVars :: IndexedTraversal Bool s t a b

instance HasKindVars (Kind a) (Kind b) a b where
  kindVars f x0 = go False x0 where
    go t (Var a)   = Var   <$> indexed f t a
    go _ (Type k)  = Type  <$> go True k
    go t (x :-> y) = (:->) <$> go t x <*> go t y
    go _ (HardKind hk) = pure $ HardKind hk

instance HasKindVars s t a b => HasKindVars [s] [t] a b where
  kindVars = traverse.kindVars

instance (HasKindVars s t a b, HasKindVars u v a b) => HasKindVars (s,u) (t,v) a b where
  kindVars = beside kindVars kindVars

instance HasKindVars s t a b => HasKindVars (IntMap s) (IntMap t) a b where
  kindVars = traverse.kindVars

instance HasKindVars s t a b => HasKindVars (Map k s) (Map k t) a b where
  kindVars = traverse.kindVars

instance HasKindVars s t a b => HasKindVars (Hinted s) (Hinted t) a b where
  kindVars = traverse.kindVars

------------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------------

-- | Kind schemas
data Schema a = Schema [Hint] !(Scope Int Kind a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable, Data, Generic)

instance Hashable a => Hashable (Schema a)
instance Hashable1 Schema
instance Digestable a => Digestable (Schema a)

instance Fun Schema where
  _Fun = prism hither yon
    where
    hither (Schema nml (Scope s), Schema nmr t) = Schema (nml ++ nmr) $
      let n = length nml
          Scope t' = mapBound (+n) t
      in Scope (s :-> t')
    yon t@(Schema nms s) = case fromScope s of
      l :-> r -> case (maximumOf (traverse._B) l, minimumOf (traverse._B) r) of
        (Nothing, Nothing)              ->
          Right (Schema [] (toScope l), Schema [] (toScope r))
        (Nothing, Just 0)               ->
          Right (Schema [] (toScope l), Schema nms (toScope r))
        (Just m, Nothing)  | length nms == m + 1 ->
          Right (Schema nms (toScope l), Schema [] (toScope r))
        (Just m, Just o)   | m == o - 1 ->
          let (nml, nmr) = splitAt o nms
           in Right (Schema nml (toScope l), Schema nmr (toScope (r & mapped._B -~ o)))
        _                               -> Left t
      _                                 -> Left t

instance Variable Schema where
  _Var = prism (schema . return) $ \ t@(Schema _ (Scope b)) -> case b of
    Var (F (Var k)) -> Right k
    _               -> Left  t

instance (k ~ Kind, k' ~ Kind, b ~ b') => HasKindVars (Scope b k a) (Scope b' k' a') a a' where
  kindVars f (Scope s) = Scope <$> icompose (||) kindVars (traverse.kindVars) f s

instance Kindly (Schema a) where
  hardKind = prism (schema . review hardKind) $ \ t@(Schema _ (Scope b)) -> case b of
    HardKind k           -> Right k
    Var (F (HardKind k)) -> Right k
    _                    -> Left t

instance HasKindVars (Schema a) (Schema b) a b where
  kindVars f (Schema hs s) = Schema hs <$> kindVars f s

instance BoundBy Schema Kind where
  boundBy f (Schema i b) = Schema i (boundBy f b)

instance Serial1 Schema where
  serializeWith pk (Schema n body) = serialize n >> serializeWith pk body
  {-# INlINE serializeWith #-}

  deserializeWith gk = liftM2 Schema deserialize (deserializeWith gk)
  {-# INLINE deserializeWith #-}

instance Binary k => Binary (Schema k) where
  put = serializeWith Binary.put
  get = deserializeWith Binary.get

instance Serialize k => Serialize (Schema k) where
  put = serializeWith Serialize.put
  get = deserializeWith Serialize.get

instance Serial k => Serial (Schema k) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

wfk, wfb :: Kind a -> Bool
wfk Var{} = True
wfk (a :-> b) = wfk a && wfk b
wfk (Type a) = wfb a
wfk (HardKind hk) = wfhk hk

wfb Var{} = True
wfb (_ :-> _) = False
wfb (Type _) = False
wfb (HardKind hb) = wfhb hb

wfhb, wfhk :: HardKind -> Bool
wfhb Star    = True
wfhb Unboxed = True
wfhb Native  = True
wfhb _       = False

wfhk = not . wfhb

-- | Lift a kind into a kind schema
--
-- >>> schema (star ~> star)
-- Schema [] (Scope (Var (F (HardKind Star :-> HardKind Star))))
schema :: Kind a -> Schema a
schema k = Schema [] (lift k)

-- | Construct a schema from a kind, generalizing all free variables.
--
-- When working with 'Ermine.Kind.Inference.Meta' variables, you want to use
-- generalize to 'zonk' the kind and check skolems.
--
-- >>> general "a" (Hinted ?? ())
-- Schema [Hinted "a" ()] (Scope (Var (B 0)))
--
-- >>> general ("a" ~> "a") (Hinted ?? ())
-- Schema [Hinted "a" ()] (Scope (Var (B 0) :-> Var (B 0)))
--
-- >>> general ("a" ~> "b") (Hinted ?? ())
-- Schema [Hinted "a" (),Hinted "b" ()] (Scope (Var (B 0) :-> Var (B 1)))
--
-- >>> general ("b" ~> "a") (Hinted ?? ())
-- Schema [Hinted "b" (),Hinted "a" ()] (Scope (Var (B 0) :-> Var (B 1)))
--
-- >>> general (star ~> star) (Hinted ?? ())
-- Schema [] (Scope (Type (HardKind Star) :-> Type (HardKind Star)))
general :: Ord k => Kind k -> (k -> Hint) -> Schema a
general k0 h = Schema (reverse hs) (Scope r) where
 ((_, hs, _), r) = mapAccumL go (Map.empty, [], 0) k0
 go mhn@(m, hl, n) k = case m^.at k of
   Just b  -> (mhn, B b)
   Nothing -> let n' = n + 1 in n' `seq` ((m & at k ?~ n, h k : hl, n'), B n)
