{-# LANGUAGE CPP #-}
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
--
--------------------------------------------------------------------
module Ermine.Kind.Inference
  ( MetaK, KindM
  , MetaT, TypeM
  , inferKind
  , checkKind
  , generalize
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Data.Foldable
import Data.Monoid
import Data.Void
import Data.IntSet as IntSet
import Data.IntMap as IntMap
import Data.STRef
import Ermine.Kind as Kind
import Ermine.Meta
import qualified Ermine.Type as Type
import           Ermine.Type hiding (Var)

type MetaK s = Meta s Kind ()
type KindM s = Kind (MetaK s)
type MetaT s = Meta s (Type (MetaK s)) (KindM s)
type TypeM s = Type (MetaK s) (MetaT s)

-- | Returns the a unified form if different from the left argument.
--
-- This enables us to increase sharing and avoid redundant write-back
unifyKind :: IntSet -> KindM s -> KindM s -> M s (Maybe (KindM s))
unifyKind is k1 k2 = do
   k1' <- semiprune k1
   k2' <- semiprune k2
   go k1' k2'
   where
     go (Var kv1) (Var kv2) | kv1 == kv2 = return Nothing
     go (Var (Meta _ i r _ u)) (Var (Meta _ i' r' _ v)) = do
       -- union-by-rank
       m <- liftST $ readSTRef u
       n <- liftST $ readSTRef v
       Just <$> if m <= n
         then unifyKV is i  r  k2 (let m' = m + 1 in m' `seq` writeSTRef u m')
         else unifyKV is i' r' k1 (let n' = n + 1 in n' `seq` writeSTRef u n')
     go (Var (Meta _ i r _ u)) k  = Just <$> unifyKV is i r k (bumpRank u)
     go k (Var (Meta _ i r _ u))  = Just <$> unifyKV is i r k (bumpRank u)
     go (a :-> b) (c :-> d) = merge <$> unifyKind is a c <*> unifyKind is b d where
       merge Nothing   Nothing   = Nothing
       merge (Just ac) Nothing   = Just (ac :-> b)
       merge Nothing   (Just bd) = Just (a  :-> bd)
       merge (Just ac) (Just bd) = Just (ac :-> bd)
     go (HardKind x) (HardKind y) | x == y = return Nothing
     go _ _ = fail "kind mismatch"

-- | We don't need to update depths in the kind variables. We only create
-- meta variables with non-rankInf rank for annotations, and annotations do
-- not, at least at this time, bind kinds.
unifyKV :: IntSet -> Int -> STRef s (Maybe (KindM s)) -> KindM s -> ST s () -> M s (KindM s)
unifyKV is i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j | is^.ix i  -> fail "kind occurs"
         | otherwise -> unifyKind (IntSet.insert i is) j k >>= \o -> liftST $ case o of
    Nothing -> return k -- nothing changed
    Just k' -> k' <$ writeSTRef r o
  Nothing -> liftST $ do
    bump
    k <$ writeSTRef r (Just k)

-- | Unify a known 'Meta' variable
unifyKindVar :: IntSet -> MetaK s -> KindM s -> M s (KindM s)
unifyKindVar is (Meta _ i r _ u) kv = unifyKV is i r kv (bumpRank u)
unifyKindVar _  (Skolem _ _)     _  = error "unifyKindVar: Skolem"

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: KindM s -> M s (KindM s, KindM s)
matchFunKind (a :-> b)    = return (a, b)
matchFunKind (HardKind _) = fail "not a fun kind"
matchFunKind (Var kv) = do
  a <- Var <$> newMeta ()
  b <- Var <$> newMeta ()
  (a, b) <$ unifyKindVar mempty kv (a :-> b)

instantiateSchema :: Schema (MetaK s) -> M s (KindM s)
instantiateSchema (Schema n s) = do
  vs <- replicateM n (Var <$> newMeta ())
  return $ instantiate (vs!!) s

checkKind :: Type (MetaK s) (KindM s) -> KindM s -> M s ()
checkKind t k = do
  k' <- inferKind t
  () <$ unifyKind mempty k k'

instantiateList :: Monad t => [a] -> Scope Int t a -> t a
instantiateList as = instantiate (return . (as !!))

inferKind :: Type (MetaK s) (KindM s) -> M s (KindM s)
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
  sks <- replicateM n $ newSkolem ()
  let btys = instantiateList sks <$> tks
  for_ cs $ \c ->
    checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys c)) constraint
  checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys b)) star
  return star

generalize :: KindM s -> M s (Schema a)
generalize k0 = do
  k <- zonk mempty k0
  (r,(_,n)) <- runStateT (traverse go k) (IntMap.empty, 0)
  return $ Schema n (Scope r)
  where
   go Skolem{}   = StateT $ \ _ -> fail "escaped skolem"
   go (Meta _ i _ _ _) = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))
