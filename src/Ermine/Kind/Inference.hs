{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Kind.Inference
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Ermine.Kind.Inference
  ( inferKind
  , checkKind
  -- * Meta variables
  , Meta(..)
  -- * Meta Prisms
  , skolem
  , meta
  -- * Meta Lenses
  , metaId
  -- * Working with Meta
  , newMeta
  , newSkolem
  , readMeta
  , writeMeta
  -- * Pruning
  , semiprune
  , zonk
  , generalize
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Data.STRef
import Data.Foldable
import Data.Function (on)
import Data.Monoid
import Data.Traversable
import Data.Void
import Data.IntSet
import Data.IntMap as IntMap
import Ermine.Kind as Kind
import Ermine.Syntax
import Ermine.Unification
import Ermine.Type as Type

-- | A meta variable for skolemization and unification
data Meta s
  = Meta !Int (STRef s (Maybe (Kind (Meta s))))
  | Skolem !Int

skolem :: Prism' (Meta s) Int
skolem = prism Skolem $ \s -> case s of
  Skolem i -> Right i
  _        -> Left s
{-# INLINE skolem #-}

meta :: Prism (Meta s)
              (Meta t)
              (Int, STRef s (Maybe (Kind (Meta s))))
              (Int, STRef t (Maybe (Kind (Meta t))))
meta = prism (uncurry Meta) $ \s -> case s of
  Meta i r -> Right (i, r)
  Skolem i -> Left (Skolem i)
{-# INLINE meta #-}

instance Show (Meta s) where
  showsPrec d (Meta n _) = showParen (d > 10) $
    showString "Meta " . showsPrec 11 n . showString " ..."
  showsPrec d (Skolem n) = showParen (d > 10) $
    showString "Skolem " . showsPrec 11 n

instance Eq (Meta s) where
  (==) = (==) `on` view metaId
  {-# INLINE (==) #-}

instance Ord (Meta s) where
  compare = compare `on` view metaId
  {-# INLINE compare #-}

metaId :: Lens' (Meta s) Int
metaId f (Meta i s) = f i <&> \j -> Meta j s
metaId f (Skolem i) = Skolem <$> f i
{-# INLINE metaId #-}

-- | Construct a new meta variable
newMeta :: U s (Meta s)
newMeta = Meta <$> fresh <*> st (newSTRef Nothing)
{-# INLINE newMeta #-}

newSkolem :: U s (Meta s)
newSkolem = Skolem <$> fresh
{-# INLINE newSkolem #-}

-- | Read a meta variable
readMeta :: Meta s -> U s (Maybe (Kind (Meta s)))
readMeta (Meta _ v) = st $ readSTRef v
readMeta (Skolem _) = return Nothing
{-# INLINE readMeta #-}

-- | Write to a meta variable
writeMeta :: Meta s -> Kind (Meta s) -> U s ()
writeMeta (Meta _ v) a = st $ writeSTRef v (Just a)
writeMeta (Skolem _) _ = fail "writeMeta: skolem"
{-# INLINE writeMeta #-}

semiprune :: Kind (Meta s) -> U s (Kind (Meta s))
semiprune t0 = case preview var t0 of
  Just v0 -> loop t0 v0
  Nothing -> return t0
  where
    loop t1 v1 = readMeta v1 >>= \mb -> case mb of
      Nothing -> return t1
      Just t  -> case preview var t of
        Nothing -> return t -- t1?
        Just v  -> do
          fv <- loop t v
          writeMeta v1 fv
          return fv
{-# INLINE semiprune #-}

-- | Expand meta variables recursively
zonk :: IntSet -> Kind (Meta s) -> U s (Kind (Meta s))
zonk is fs = fmap join . for fs $ \m -> readMeta m >>= \mv -> case mv of
  Nothing  -> return (return m)
  Just fmf
    | is^.ix (m^.metaId) -> fail "zonk: kind occurs"
    | otherwise -> do
    r <- zonk (is & ix (m^.metaId) .~ True) fmf -- recursively expand
    writeMeta m r                           -- save the zonked version back to avoid work later
    return r

unifyKind :: IntSet -> Kind (Meta s) -> Kind (Meta s) -> U s (Kind (Meta s))
unifyKind is k1 k2 = do
   k1' <- semiprune k1
   k2' <- semiprune k2
   go k1' k2'
   where
     go k@(Kind.Var kv1) (Kind.Var kv2)
        | kv1 == kv2 = return k
     go (Kind.Var kv1@Meta{}) k = unifyKindVar is kv1 k
     go k (Kind.Var kv2@Meta{}) = unifyKindVar is kv2 k
     go (a :-> b) (c :-> d) = (:->) <$> unifyKind is a c <*> unifyKind is b d
     go k@(HardKind x) (HardKind y)
        | x == y = return k
     go _ _ = fail "kind mismatch"

unifyKindVar :: IntSet -> Meta s -> Kind (Meta s) -> U s (Kind (Meta s))
unifyKindVar is kv k = readMeta kv >>= \mb -> case mb of
  Just j  | is^.ix (kv^.metaId) -> fail "kind occurs"
          | otherwise           -> unifyKind (is & ix (kv^.metaId) .~ True) j k
  Nothing -> k <$ writeMeta kv k

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: Kind (Meta s) -> U s (Kind (Meta s), Kind (Meta s))
matchFunKind (a :-> b)     = return (a, b)
matchFunKind (HardKind _)  = fail "not a fun kind"
matchFunKind (Kind.Var kv) = do
  a <- Kind.Var <$> newMeta
  b <- Kind.Var <$> newMeta
  (a, b) <$ unifyKindVar mempty kv (a :-> b)

instantiateSchema :: Schema (Meta s) -> U s (Kind (Meta s))
instantiateSchema (Schema n s) = do
  vs <- replicateM n (Kind.Var <$> newMeta)
  return $ instantiate (vs!!) s

checkKind :: Type (Meta s) (Kind (Meta s)) -> Kind (Meta s) -> U s ()
checkKind t k = do
  k' <- inferKind t
  () <$ unifyKind mempty k k'

instantiateList :: Monad t => [a] -> Scope Int t a -> t a
instantiateList as = instantiate (return . (as !!))

inferKind :: Type (Meta s) (Kind (Meta s)) -> U s (Kind (Meta s))
inferKind (Loc l t)                = local (const l) $ inferKind t
inferKind (Type.Var tk)            = return tk
inferKind (HardType Arrow)         = return $ star :-> star :-> star
inferKind (HardType (Con _ s))     = instantiateSchema (vacuous s)
inferKind (HardType (Tuple n))     = return $ productKind n
inferKind (HardType ConcreteRho{}) = return rho
inferKind (App f x) = do
  kf <- inferKind f
  (a, b) <- matchFunKind kf
  checkKind x a
  return b
inferKind (Exists ks cs) = do
  for_ cs $ \c ->
    checkKind (instantiateList ks c) constraint
  return constraint
inferKind (Forall n tks cs b) = do
  sks <- replicateM n newSkolem
  let btys = instantiateList sks <$> tks
  for_ cs $ \c ->
    checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys c)) constraint
  checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys b)) star
  return star

generalize :: Kind (Meta s) -> U s (Schema a)
generalize k0 = do
  k <- zonk mempty k0
  (r,(_,n)) <- runStateT (traverse go k) (IntMap.empty, 0)
  return $ Schema n (Scope r)
  where
   go Skolem{}   = StateT $ \ _ -> fail "escaped skolem"
   go (Meta i _) = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))
