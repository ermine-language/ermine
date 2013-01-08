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
  , sharing
  , inferKind
  , checkKind
  , unifyKind
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
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.Void
import Data.IntSet as IntSet
import Data.IntMap as IntMap
import Data.STRef
import Ermine.Kind as Kind
import Ermine.Meta
import qualified Ermine.Type as Type
import           Ermine.Type hiding (Var)

-- | A kind meta-variable
type MetaK s = Meta s Kind ()

-- | A kind filled with meta-variables
type KindM s = Kind (MetaK s)

-- | A type meta-variable
type MetaT s = Meta s (Type (MetaK s)) (KindM s)

-- | A type filled with meta-variables
type TypeM s = Type (MetaK s) (MetaT s)

-- | Run an action, if it returns @Any True@ then use its new value, otherwise use the passed in value.
--
-- This can be used to recover sharing during unification when no interesting unification takes place.
sharing :: Monad m => a -> WriterT Any m a -> m a
sharing a m = do
  (b, Any modified) <- runWriterT m
  return $ if modified then b else a
{-# INLINE sharing #-}

-- | Returns the a unified form if different from the left argument.
--
-- The writer is used to track if any interesting edits have been made
unifyKind :: IntSet -> KindM s -> KindM s -> WriterT Any (M s) (KindM s)
unifyKind is k1 k2 = do
  k1' <- lift (semiprune k1)
  k2' <- lift (semiprune k2)
  go k1' k2'
  where
    go k@(Var kv1) (Var kv2) | kv1 == kv2 = return k -- boring
    go a@(Var (Meta _ i r _ u)) b@(Var (Meta _ j s _ v)) = do
      -- union-by-rank
      m <- liftST $ readSTRef u
      n <- liftST $ readSTRef v
      if m <= n
        then unifyKV is i r b $ writeSTRef v $! n + 1
        else unifyKV is j s a $ writeSTRef u $! m + 1
    go (Var (Meta _ i r _ _)) k    = unifyKV is i r k $ return ()
    go k (Var (Meta _ i r _ _))    = unifyKV is i r k $ return ()
    go (a :-> b) (c :-> d)         = (:->) <$> unifyKind is a c <*> unifyKind is b d
    go k@(HardKind x) (HardKind y) | x == y = return k -- boring
    go _ _ = fail "kind mismatch"

-- | We don't need to update depths in the kind variables. We only create
-- meta variables with non-rankInf rank for annotations, and annotations do
-- not, at least at this time, bind kinds.
unifyKV :: IntSet -> Int -> STRef s (Maybe (KindM s)) -> KindM s -> ST s () -> WriterT Any (M s) (KindM s)
unifyKV is i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j | is^.ix i  -> fail "kind occurs"
         | otherwise -> do
    (k', Any m) <- listen $ unifyKind (IntSet.insert i is) j k
    if m then liftST $ k' <$ writeSTRef r (Just k') -- write back interesting changes
         else j <$ tell (Any True)                  -- short-circuit
  Nothing -> do
    tell (Any True)
    liftST $ do
      bump
      k <$ writeSTRef r (Just k)

-- | Unify a known 'Meta' variable with a kind that isn't a 'Var'.
unifyKindVar :: IntSet -> MetaK s -> KindM s -> WriterT Any (M s) (KindM s)
unifyKindVar is (Meta _ i r _ _) kv = unifyKV is i r kv $ return ()
unifyKindVar _  (Skolem _ _)     _  = error "unifyKindVar: Skolem"
{-# INLINE unifyKindVar #-}

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: KindM s -> M s (KindM s, KindM s)
matchFunKind (a :-> b)    = return (a, b)
matchFunKind (HardKind _) = fail "not a fun kind"
matchFunKind (Var kv) = do
  a <- Var <$> newMeta ()
  b <- Var <$> newMeta ()
  (a, b) <$ runWriterT (unifyKindVar mempty kv (a :-> b))

instantiateSchema :: Schema (MetaK s) -> M s (KindM s)
instantiateSchema (Schema n s) = do
  vs <- replicateM n (Var <$> newMeta ())
  return $ instantiate (vs!!) s
{-# INLINE instantiateSchema #-}

checkKind :: Type (MetaK s) (KindM s) -> KindM s -> M s ()
checkKind t k = do
  k' <- inferKind t
  () <$ runWriterT (unifyKind mempty k k')

instantiateList :: Monad t => [a] -> Scope Int t a -> t a
instantiateList as = instantiate (return . (as !!))
{-# INLINE instantiateList #-}

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
  b <$ checkKind x a
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
