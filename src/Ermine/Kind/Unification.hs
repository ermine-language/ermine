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
  , unifyKind
  , unifyKindVar
  , kindOccurs
  , generalize
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Set as Set
import Data.STRef
import Ermine.Kind as Kind
import Ermine.Meta

-- $setup
-- >>> import Ermine.Syntax
-- >>> import Text.Trifecta.Rendering

-- | A kind meta-variable
type MetaK s = Meta s Kind ()

-- | A kind filled with meta-variables
type KindM s = Kind (MetaK s)

-- | Die with an error message due to a cycle between the specified kinds.
--
-- >>> runM emptyRendering $ do k <- Var <$> newMeta (); runWriterT $ unifyKind mempty k (star ~> k); generalize k
-- Left (interactive):1:1: error: kind occurs fromList [Meta () 0 ...]
kindOccurs :: Set (MetaK s) -> M s a
kindOccurs zs = fail $ "kind occurs " ++ show zs

-- | Generalize a 'Kind', checking for escaped Skolems.
generalize :: KindM s -> M s (Schema a)
generalize k0 = do
  k <- zonk mempty k0 kindOccurs
  (r,(_,n)) <- runStateT (traverse go k) (IntMap.empty, 0)
  return $ Schema n (Scope r)
  where
   go Skolem{}   = StateT $ \ _ -> fail "escaped skolem"
   go (Meta _ i _ _ _) = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))


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
        LT -> unifyKV is True i r b $ return ()
        EQ -> unifyKV is True i r b $ writeSTRef v $! n + 1
        GT -> unifyKV is False j s a $ return ()
    go (Var (Meta _ i r _ _)) k    = unifyKV is True i r k $ return ()
    go k (Var (Meta _ i r _ _))    = unifyKV is False i r k $ return ()
    go (a :-> b) (c :-> d)         = (:->) <$> unifyKind is a c <*> unifyKind is b d
    go k@(HardKind x) (HardKind y) | x == y = return k -- boring
    go _ _ = fail "kind mismatch"

-- | We don't need to update depths in the kind variables. We only create
-- meta variables with non-rankInf rank for annotations, and annotations do
-- not, at least at this time, bind kinds.
unifyKV :: IntSet -> Bool -> Int -> STRef s (Maybe (KindM s)) -> KindM s -> ST s () -> WriterT Any (M s) (KindM s)
unifyKV is interesting i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j | is^.ix i  -> lift $ foldMap (cycles is) j >>= kindOccurs
         | otherwise -> do
    (k', Any m) <- listen $ unifyKind (IntSet.insert i is) j k
    if m then liftST $ k' <$ writeSTRef r (Just k') -- write back interesting changes
         else j <$ tell (Any True)                  -- short-circuit
  Nothing -> do
    tell (Any interesting)
    liftST $ do
      bump
      k <$ writeSTRef r (Just k)

-- | Unify a known 'Meta' variable with a kind that isn't a 'Var'.
unifyKindVar :: IntSet -> MetaK s -> KindM s -> WriterT Any (M s) (KindM s)
unifyKindVar is (Meta _ i r _ _) kv = unifyKV is True i r kv $ return ()
unifyKindVar _  (Skolem _ _)     _  = fail "unifyKindVar: Skolem"
{-# INLINE unifyKindVar #-}
