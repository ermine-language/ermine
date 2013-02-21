{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Unification.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module tells us how to unify types. We use a simplified form
-- of HMF-style unification to deal with unification of foralls.
--
--------------------------------------------------------------------
module Ermine.Unification.Type
  ( MetaT, TypeM
  , unifyType
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.IntSet as IntSet
import Data.Set as Set
import Data.IntMap as IntMap
import Data.Set.Lens
import Data.STRef
import Data.Traversable
import Ermine.Diagnostic
import Ermine.Pretty
import Ermine.Pretty.Kind
import Ermine.Pretty.Type
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import Ermine.Unification.Sharing

-- | A type meta-variable
type MetaT s = Meta s (Type (MetaK s)) (KindM s)

-- | A type filled with meta-variables
type TypeM s = Type (MetaK s) (MetaT s)

typeOccurs :: (MonadWriter Any m, MonadMeta s m) => Int -> TypeM s -> (MetaT s -> Bool) -> m (TypeM s)
typeOccurs depth1 t p = zonkWith t tweak where
  tweak m
    | p m = do
      zt <- sharing t $ zonk t
      let st = setOf typeVars zt
          mt = IntMap.fromList $ zipWith (\m n -> (m^.metaId, n)) (Set.toList st) names
          sk = setOf kindVars zt
          mk = IntMap.fromList $ zipWith (\m n -> (m^.metaId, n)) (Set.toList sk) names
          v = mt ^?! ix (st ^?! folded.filtered p.metaId)
      td <- prettyType zt (drop (Set.size st) names) (-1)
         (\v _ -> pure $ text $ mk ^?! ix (v^.metaId))
         (\v _ -> pure $ text $ mt ^?! ix (v^.metaId))
      r <- view rendering
      throwM $ die r "infinite type detected" & footnotes .~ [text "cyclic type:" <+> hang 4 (group (pretty v </> char '=' </> td))]
    | otherwise = liftST $ forMOf_ metaDepth m $ \d -> do
        depth2 <- readSTRef d
        Var m <$ when (depth2 > depth1) (writeSTRef d depth1)

-- | Unify two types, with access to a visited set, logging to a Writer whether or not the answer differs from the first type argument.
unifyType :: (MonadWriter Any m, MonadMeta s m) => TypeM s -> TypeM s -> m (TypeM s)
unifyType t1 t2 = do
  t1' <- semiprune t1
  t2' <- semiprune t2
  go t1' t2'
  where
    go x@(Var tv1)                (Var tv2)              | tv1 == tv2 = return x
    go x@(Var (Meta _ i r d u)) y@(Var (Meta _ j s e v)) = do
       -- union-by-rank
       m <- liftST $ readSTRef u
       n <- liftST $ readSTRef v
       case compare m n of
         LT -> unifyTV True i r d y $ return ()
         EQ -> unifyTV True i r d y $ writeSTRef v $! n + 1
         GT -> unifyTV False j s e x $ return ()
    go (Var (Meta _ i r d _)) t                       = unifyTV True i r d t $ return ()
    go t                      (Var (Meta _ i r d _))  = unifyTV False i r d t $ return () -- not as boring as it could be
    go (App f x)              (App g y)               = App <$> unifyType f g <*> unifyType x y
    go (Loc l s)              t                       = Loc l <$> unifyType s t
    go s                      (Loc _ t)               = unifyType s t
    go Exists{}               _                       = fail "unifyType: existential"
    go _                      Exists{}                = fail "unifyType: existential"
    go t@(Forall m xs _ a)   (Forall n ys _ b)
      | m /= n                 = fail "unifyType: forall: mismatched kind arity"
      | length xs /= length ys = fail "unifyType: forall: mismatched type arity"
      | otherwise = do
        (_, Any modified) <- listen $ do
          sks <- replicateM m $ newSkolem ()
          let nxs = instantiateVars sks <$> xs
              nys = instantiateVars sks <$> ys
          sts <- for (zip nxs nys) $ \(x,y) -> do
            k <- unifyKind x y
            newSkolem k
          unifyType (instantiateKindVars sks (instantiateVars sts a))
                    (instantiateKindVars sks (instantiateVars sts b))
        if modified then zonk t
                    else return t
    go t@(HardType x) (HardType y) | x == y = return t
    go _ _ = fail "type mismatch"
{-# INLINE unifyType #-}

unifyTV :: (MonadWriter Any m, MonadMeta s m) => Bool -> Int -> STRef s (Maybe (TypeM s)) -> STRef s Depth -> TypeM s -> ST s () -> m (TypeM s)
unifyTV interesting i r d t bump = liftST (readSTRef r) >>= \ mt1 -> case mt1 of
  Just j -> do
    (t', Any m) <- listen $ unifyType j t
    if m then liftST $ t' <$ writeSTRef r (Just t')
         else j <$ tell (Any True)
  Nothing -> case t of
    Var (Meta _ _ _ e _) -> do -- this has been semipruned so its not part of a chain
      tell (Any interesting)
      liftST $ do
        bump
        writeSTRef r (Just t)
        depth1 <- readSTRef d
        depth2 <- readSTRef e
        when (depth2 > depth1) $ writeSTRef e depth1
        return t
    _ -> do
      tell (Any interesting)
      depth1 <- liftST $ readSTRef d
      zt <- typeOccurs depth1 t $ \v -> v^.metaId == i
      zt <$ liftST (writeSTRef r (Just zt))
