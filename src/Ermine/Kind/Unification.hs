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
-- Module    :  Ermine.Kind.Unification
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Kind.Unification
  ( MetaK, KindM
  , MetaT, TypeM
  , unifyKind
  , unifyKindVar
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IntSet as IntSet
import Data.STRef
import Ermine.Kind as Kind
import Ermine.Meta
import Ermine.Type (Type)

-- | A kind meta-variable
type MetaK s = Meta s Kind ()

-- | A kind filled with meta-variables
type KindM s = Kind (MetaK s)

-- | A type meta-variable
type MetaT s = Meta s (Type (MetaK s)) (KindM s)

-- | A type filled with meta-variables
type TypeM s = Type (MetaK s) (MetaT s)

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
      case compare m n of
        LT -> unifyKV is i r b $ return ()
        EQ -> unifyKV is i r b $ writeSTRef v $! n + 1
        GT -> unifyKV is j s a $ return ()
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
