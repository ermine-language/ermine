{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Unification.Kind
  ( unifyKind
  , unifyKindVar
  , kindOccurs
  , generalize
  , generalizeOver
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IntSet as IntSet
import Data.IntMap as IntMap
import Data.Set as Set
import Data.Set.Lens
import Data.STRef
import Ermine.Diagnostic
import Ermine.Syntax.Hint
import Ermine.Syntax.Kind as Kind
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Ermine.Pretty
import Ermine.Pretty.Kind

-- $setup
-- >>> :set -XFlexibleContexts -XConstraintKinds -XTypeFamilies
-- >>> import Ermine.Syntax
-- >>> import Text.Trifecta.Rendering
-- >>> let test :: (forall m s. (MonadWriter Any m, MonadMeta s m) => m (KindM s)) -> Schema b; test mk = fst $ runM_ emptyRendering (runWriterT (mk >>= generalize))

-- | Die with an error message due to a cycle between the specified kinds.
--
-- >>> test $ do k <- Var <$> newMeta (); unifyKind k (star ~> k)
-- *** Exception: (interactive):1:1: error: infinite kind detected
-- cyclic kind: a = * -> a
--
-- >>> test $ do k1 <- Var <$> newMeta (); k2 <- Var <$> newMeta (); unifyKind k1 (k1 ~> k2); unifyKind k2 (k1 ~> k2); return (k1 ~> k2)
-- *** Exception: (interactive):1:1: error: infinite kind detected
-- cyclic kind: a = a -> b
kindOccurs :: (MonadMeta s m, MonadWriter Any m) => KindM s -> (MetaK s -> Bool) -> m (KindM s)
kindOccurs k p = occurs k p $ do
  zk <- zonk k
  let s = setOf kindVars zk
      m = IntMap.fromList $ zipWith (\u n -> (u^.metaId, n)) (Set.toList s) names
      v = m ^?! ix (s ^?! folded.filtered p.metaId)
      kd = prettyKind (fmap (\u -> m ^?! ix (u^.metaId)) zk) False
  r <- viewMeta rendering
  throwM $ die r "infinite kind detected" & footnotes .~ [text "cyclic kind:" <+> hang 4 (group (pretty v </> char '=' </> kd))]
{-# INLINE kindOccurs #-}

-- | Generalize a 'Kind', checking for escaped Skolems.
generalize :: MonadMeta s m => KindM s -> m (Schema a)
generalize = generalizeOver IntSet.empty

generalizeOver :: MonadMeta s m => IntSet -> KindM s -> m (Schema a)
generalizeOver sks k0 = do
  k <- runSharing k0 $ zonk k0
  (r,(_,n)) <- runStateT (traverse go k) (IntMap.empty, 0)
  return $ Schema (replicate n $ Unhinted ()) (Scope r)
  where
   go s@Skolem{}
     | not $ sks^.contains (s^.metaId) = StateT $ \ _ -> fail "escaped skolem"
   go m = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))
    where i = m ^. metaId

-- | Returns the a unified form if different from the left argument.
--
-- The writer is used to track if any interesting edits have been made
unifyKind :: (MonadMeta s m, MonadWriter Any m) => KindM s -> KindM s -> m (KindM s)
unifyKind k1 k2 = do
  k1' <- semiprune k1
  k2' <- semiprune k2
  go k1' k2'
  where
    go k@(Var kv1) (Var kv2) | kv1 == kv2 = return k -- boring
    go a@(Var (Meta _ i r _ u)) b@(Var (Meta _ j s _ v)) = do
      -- union-by-rank
      m <- liftST $ readSTRef u
      n <- liftST $ readSTRef v
      case compare m n of
        LT -> unifyKV True i r b $ return ()
        EQ -> unifyKV True i r b $ writeSTRef v $! n + 1
        GT -> unifyKV False j s a $ return ()
    go (Var (Meta _ i r _ _)) k    = unifyKV True i r k $ return ()
    go k (Var (Meta _ i r _ _))    = unifyKV False i r k $ return ()
    go (a :-> b) (c :-> d)         = (:->) <$> unifyKind a c <*> unifyKind b d
    go k@(HardKind x) (HardKind y) | x == y = return k -- boring
    go _ _ = fail "kind mismatch"

-- | We don't need to update depths in the kind variables. We only create
-- meta variables with non-rankInf rank for annotations, and annotations do
-- not, at least at this time, bind kinds.
unifyKV :: (MonadMeta s m, MonadWriter Any m) => Bool -> Int -> STRef s (Maybe (KindM s)) -> KindM s -> ST s () -> m (KindM s)
unifyKV interesting i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j -> do
    (k', Any m) <- listen $ unifyKind j k
    if m then liftST $ k' <$ writeSTRef r (Just k') -- write back interesting changes
         else j <$ tell (Any True)                  -- short-circuit
  Nothing -> do
    zk <- kindOccurs k $ \v -> v^.metaId == i
    tell (Any interesting)
    liftST $ do
      bump
      zk <$ writeSTRef r (Just zk)

-- | Unify a known 'Meta' variable with a kind that isn't a 'Var'.
unifyKindVar :: (MonadMeta s m, MonadWriter Any m) => MetaK s -> KindM s -> m (KindM s)
unifyKindVar (Meta _ i r _ _) kv = unifyKV True i r kv $ return ()
unifyKindVar (Skolem _ _)     _  = fail "unifyKindVar: Skolem"
{-# INLINE unifyKindVar #-}
