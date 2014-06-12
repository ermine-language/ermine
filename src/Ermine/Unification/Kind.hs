{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011-2014
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
-- >>> :set -XFlexibleContexts -XConstraintKinds -XTypeFamilies -XRankNTypes
-- >>> import Ermine.Syntax
-- >>> import Text.Trifecta.Rendering
-- >>> import Control.Comonad
-- >>> let test :: (forall m s. (MonadWriter Any m, MonadMeta s m) => m (KindM s)) -> Schema b; test mk = extract $ runM_ emptyRendering (unsharingT (mk >>= generalize))

-- | Die with an error message due to a cycle between the specified kinds.
--
-- >>> test $ do k <- Var <$> newMeta False; unifyKind k (star ~> k)
-- *** Exception: (interactive):1:1: error: infinite kind detected
-- cyclic kind: a = * -> a
--
-- >>> test $ do k1 <- Var <$> newMeta False; k2 <- Var <$> newMeta False; unifyKind k1 (k1 ~> k2); unifyKind k2 (k1 ~> k2); return (k1 ~> k2)
-- *** Exception: (interactive):1:1: error: infinite kind detected
-- cyclic kind: a = a -> b
kindOccurs
  :: (MonadMeta s m, MonadWriter Any m)
  => Depth -> KindM s -> (MetaK s -> Bool) -> m (KindM s)
kindOccurs depth1 k p = zonkWith k tweak where
  tweak m
    | p m = do
      zk <- sharing k $ zonk k
      let sk = setOf kindVars zk
          mk = IntMap.fromList $ zipWith (\u n -> (u^.metaId, n)) (Set.toList sk) names
          v = mk ^?! ix (sk ^?! folded.filtered p.metaId)
          kd = prettyKind (fmap (\u -> mk ^?! ix (u^.metaId)) zk) False
      r <- viewMeta rendering
      throwM $ die r "infinite kind detected" & footnotes .~ [text "cyclic kind:" <+> hang 4 (group (pretty v </> char '=' </> kd))]
    | otherwise = liftST $ forMOf_ metaDepth m $ \d -> do
        depth2 <- readSTRef d
        Var m <$ when (depth2 > depth1) (writeSTRef d depth1)
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
unifyKind
  :: (MonadMeta s m, MonadWriter Any m)
  => KindM s -> KindM s -> m (KindM s)
unifyKind k1 k2 = do
  k1' <- semiprune k1
  k2' <- semiprune k2
  go k1' k2'
  where
    go k@(Var kv1) (Var kv2) | kv1 == kv2 = return k -- boring
    go a@(Var (Meta _ _ i r d u)) b@(Var (Meta _ _ j s e v)) = do
      -- union-by-rank
      m <- liftST $ readSTRef u
      n <- liftST $ readSTRef v
      case compare m n of
        LT -> unifyKV True i r d b $ return ()
        EQ -> unifyKV True i r d b $ writeSTRef v $! n + 1
        GT -> unifyKV False j s e a $ return ()
    go (Var (Meta _ _ i r d _)) k    = unifyKV True i r d k $ return ()
    go k (Var (Meta _ _ i r d _))    = unifyKV False i r d k $ return ()
    go (a :-> b) (c :-> d)         = (:->) <$> unifyKind a c <*> unifyKind b d
    go k@(HardKind x) (HardKind y) | x == y = return k -- boring
    go (Type k) (Type k')          = Type <$> unifyKind k k'
    go x y = fail $ "kind mismatch: " ++ show x ++ " /= " ++ show y

-- | We don't need to update depths in the kind variables. We only create
-- meta variables with non-depthInf rank for annotations, and annotations do
-- not, at least at this time, bind kinds.
unifyKV
  :: (MonadMeta s m, MonadWriter Any m)
  => Bool -> Int -> STRef s (Maybe (KindM s)) -> STRef s Depth
  -> KindM s -> ST s () -> m (KindM s)
unifyKV interesting i r d k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j -> do
    (k', Any m) <- listen $ unifyKind j k
    if m then liftST $ k' <$ writeSTRef r (Just k') -- write back interesting changes
         else j <$ tell (Any True)                  -- short-circuit
  Nothing -> case k of
    Var (Meta _ _ _ _ e _) -> do
      tell (Any interesting)
      liftST $ do
        bump
        writeSTRef r (Just k)
        depth1 <- readSTRef d
        depth2 <- readSTRef e
        when (depth2 > depth1) $ writeSTRef e depth1
        return k
    _ -> do
      tell (Any interesting)
      depth1 <- liftST $ readSTRef d
      zt <- kindOccurs depth1 k $ \v -> v^.metaId == i
      zt <$ liftST (writeSTRef r (Just zt))

-- | Unify a known 'Meta' variable with a kind that isn't a 'Var'.
unifyKindVar
  :: (MonadMeta s m, MonadWriter Any m)
  => MetaK s -> KindM s -> m (KindM s)
unifyKindVar (Meta _ _ i r d _) kv = unifyKV True i r d kv $ return ()
unifyKindVar Skolem{} _ = fail "unifyKindVar: Skolem"
{-# INLINE unifyKindVar #-}
