{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Type.Unification
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
module Ermine.Type.Unification
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
import Data.STRef
import Data.Traversable
import Ermine.Kind.Unification
import Ermine.Meta
import Ermine.Scope
import Ermine.Type as Type

-- | A type meta-variable
type MetaT s = Meta s (Type (MetaK s)) (KindM s)

-- | A type filled with meta-variables
type TypeM s = Type (MetaK s) (MetaT s)

typeOccurs :: MonadMeta s m => Set (MetaT s) -> m a
typeOccurs zs = fail $ "type occurs " ++ show zs

-- | Unify two types, with access to a visited set, logging to a Writer whether or not the answer differs from the first type argument.
unifyType :: (MonadWriter Any m, MonadMeta s m) => IntSet -> TypeM s -> TypeM s -> m (TypeM s)
unifyType is t1 t2 = do
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
         LT -> unifyTV is True i r d y $ return ()
         EQ -> unifyTV is True i r d y $ writeSTRef v $! n + 1
         GT -> unifyTV is False j s e x $ return ()
    go (Var (Meta _ i r d _)) t                       = unifyTV is True i r d t $ return ()
    go t                      (Var (Meta _ i r d _))  = unifyTV is False i r d t $ return () -- not as boring as it could be
    go (App f x)              (App g y)               = App <$> unifyType is f g <*> unifyType is x y
    go (Loc l s)              t                       = Loc l <$> unifyType is s t
    go s                      (Loc _ t)               = unifyType is s t
    go Exists{}               _                       = fail "unifyType: existential"
    go _                      Exists{}                = fail "unifyType: existential"
    go t@(Forall m xs [] a)  (Forall n ys [] b)
      | m /= n                 = fail "unifyType: forall: mismatched kind arity"
      | length xs /= length ys = fail "unifyType: forall: mismatched type arity"
      | otherwise = do
        (_, Any modified) <- listen $ do
          sks <- replicateM m $ newSkolem ()
          let nxs = instantiateVars sks <$> xs
              nys = instantiateVars sks <$> ys
          sts <- for (zip nxs nys) $ \(x,y) -> do
            k <- unifyKind mempty x y
            newSkolem k
          unifyType is (instantiateKindVars sks (instantiateVars sts a))
                       (instantiateKindVars sks (instantiateVars sts b))
        if modified then zonk is t typeOccurs
                    else return t
    go t@(HardType x) (HardType y) | x == y = return t
    go _ _ = fail "type mismatch"

unifyTV :: (MonadWriter Any m, MonadMeta s m) => IntSet -> Bool -> Int -> STRef s (Maybe (TypeM s)) -> STRef s Depth -> TypeM s -> ST s () -> m (TypeM s)
unifyTV is interesting i r d t bump = liftST (readSTRef r) >>= \ mt1 -> case mt1 of
  Just j | is^.ix i -> cycles is j >>= typeOccurs
         | otherwise -> do
    (t', Any m) <- listen $ unifyType (IntSet.insert i is) j t
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
      if depth1 < maxBound
        then adjustDepth (IntSet.insert i is) depth1 t
        else return t -- we're done, zonk remaining cycles later

adjustDepth :: (MonadWriter Any m, MonadMeta s m) => IntSet -> Depth -> TypeM s -> m (TypeM s)
adjustDepth is depth1 = fmap join . traverse go where
  go v@(Meta _ j s d _)
    | is^.ix j  = fail "type occurs"
    | otherwise = liftST (readSTRef s) >>= \ mt -> case mt of
    Just t -> do -- we found a target
      tell (Any True) -- we're zonking
      t' <- adjustDepth (IntSet.insert j is) depth1 t -- chase children
      liftST $ t' <$ writeSTRef s (Just t')
    Nothing -> liftST $ do -- we didn't find a target, adjust the depth of this variable
      depth2 <- readSTRef d
      Var v <$ when (depth2 > depth1) (writeSTRef d depth1) -- boring
  go v = return (pure v)
