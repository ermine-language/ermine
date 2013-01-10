{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Arrow hiding ((<+>))
import Control.Lens
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Set as Set
import Data.STRef
import Data.Traversable
import Ermine.Diagnostic
import Ermine.Kind as Kind
import Ermine.Meta
import Ermine.Pretty

-- $setup
-- >>> :set -XFlexibleContexts -XConstraintKinds -XTypeFamilies
-- >>> import Ermine.Syntax
-- >>> import Text.Trifecta.Rendering
-- >>> let test :: (forall m s. (MonadWriter Any m, MonadMeta s m) => m (KindM s)) -> Schema b; test mk = fst $ runM_ emptyRendering (runWriterT (mk >>= generalize))

-- | A kind meta-variable
type MetaK s = Meta s Kind ()

-- | A kind filled with meta-variables
type KindM s = Kind (MetaK s)

data Occ = Occ { _occVar :: IntMap String, _occFresh :: [String] }
makeLenses ''Occ

-- | Die with an error message due to a cycle between the specified kinds.
--
-- >>> test $ do k <- Var <$> newMeta (); unifyKind mempty k (star ~> k)
-- *** Exception: (interactive):1:1: error: infinite kind detected
-- cyclic kind: a = * -> a
--
-- >>> test $ do k1 <- Var <$> newMeta (); k2 <- Var <$> newMeta (); unifyKind mempty k1 (k1 ~> k2); unifyKind mempty k2 (k1 ~> k2); return (k1 ~> k2)
-- *** Exception: (interactive):1:1: error: infinite kinds detected in (a -> b)
-- cyclic kind: a = a -> b
-- cyclic kind: b = a -> b
kindOccurs :: MonadMeta s m => Set (MetaK s) -> m a
kindOccurs zs = evalStateT ?? Occ mempty names $ do
  ns  <- for (Set.toList zs) $ \z -> occVar.at (z^.metaId).non "" <<~ occFresh %%= (head &&& tail)
  rhs <- for (Set.toList zs) $ \z -> lift (readMeta z) >>= \mk -> case mk of
    Nothing -> fail "the impossible happened: unbound cyclic kind-meta-variable"
    Just k  -> prettyKind k False walk
  let docs = zipWith ?? ns ?? rhs $ \n r -> text "cyclic kind:" <+> hang 4 (group (pretty n </> char '=' </> r))
  r <- view rendering
  let msg | length ns == 1 = "infinite kind detected"
          | otherwise      = "infinite kinds detected"
  throwM $ die r msg & aux .~ docs
  where
    -- walk :: MetaK s -> Bool -> StateT Occ (M s) TermDoc
    walk v _ | zs^.ix v = unbound v
    walk v b = readMeta v >>= \mk -> case mk of
      Nothing -> unbound v
      Just k -> prettyKind k b walk

    -- unbound :: MetaK s -> StateT Occ (M s) TermDoc
    unbound v | i <- v^.metaId = use (occVar.at i) >>= \ mn -> case mn of
      Just n -> return $ pretty n
      Nothing -> do
        n <- occVar.at i.non "" <<~ occFresh %%= (head &&& tail)
        return $ pretty n

-- | Generalize a 'Kind', checking for escaped Skolems.
generalize :: MonadMeta s m => KindM s -> m (Schema a)
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
unifyKind :: (MonadMeta s m, MonadWriter Any m) => IntSet -> KindM s -> KindM s -> m (KindM s)
unifyKind is k1 k2 = do
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
unifyKV :: (MonadMeta s m, MonadWriter Any m) => IntSet -> Bool -> Int -> STRef s (Maybe (KindM s)) -> KindM s -> ST s () -> m (KindM s)
unifyKV is interesting i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j | is^.ix i  -> cycles is j >>= kindOccurs
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
unifyKindVar :: (MonadMeta s m, MonadWriter Any m) => IntSet -> MetaK s -> KindM s -> m (KindM s)
unifyKindVar is (Meta _ i r _ _) kv = unifyKV is True i r kv $ return ()
unifyKindVar _  (Skolem _ _)     _  = fail "unifyKindVar: Skolem"
{-# INLINE unifyKindVar #-}
