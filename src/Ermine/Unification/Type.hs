{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
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
  ( unifyType
  , zonkKindsAndTypes
  , zonkKindsAndTypesWith
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.Bitraversable
import Data.Set as Set
import Data.IntMap as IntMap
import Data.Set.Lens
import Data.STRef
import Data.Traversable
import Ermine.Diagnostic
import Ermine.Pretty
import Ermine.Pretty.Type
import Ermine.Inference.Kind (checkKind)
import Ermine.Syntax.Scope
import Ermine.Syntax.Type as Type
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import Ermine.Unification.Sharing

-- | Perform an occurs check against a type.
--
-- This rules out infinite circular type signatures.
--
-- This returns the fully zonked 'Type' as a consequence, since it needs it anyways.
typeOccurs
  :: (MonadWriter Any m, MonadMeta s m)
  => Depth -> TypeM s -> (MetaT s -> Bool) -> m (TypeM s)
typeOccurs depth1 t p = zonkKindsAndTypesWith t tweakDepth tweakType where
  tweakDepth :: MonadST m => Meta (World m) f a -> m ()
  tweakDepth m = liftST $ forMOf_ metaDepth m $ \d -> do
    depth2 <- readSTRef d
    when (depth2 > depth1) $ writeSTRef d depth1
  tweakType m
    | p m = do
      zt <- sharing t $ zonk t
      let st = setOf typeVars zt
          mt = IntMap.fromList $ zipWith (\u n -> (u^.metaId, n)) (Set.toList st) names
          sk = setOf kindVars zt
          mk = IntMap.fromList $ zipWith (\u n -> (u^.metaId, n)) (Set.toList sk) names
          v = mt ^?! ix (st ^?! folded.filtered p.metaId)
          td = prettyType
                 (bimap (\u -> mk ^?! ix (u^.metaId))
                        (\u -> mt ^?! ix (u^.metaId)) zt)
                 (drop (Set.size st) names)
                 (-1)
      r <- viewMeta rendering
      throwM $ die r "infinite type detected" & footnotes .~ [text "cyclic type:" <+> hang 4 (group (pretty v </> char '=' </> td))]
    | otherwise = tweakDepth m

zonkKindsAndTypes
  :: (MonadMeta s m, MonadWriter Any m)
  => TypeM s -> m (TypeM s)
zonkKindsAndTypes t = zonkKindsAndTypesWith t (const $ return ()) (const $ return ())

zonkKindsAndTypesWith
  :: (MonadMeta s m, MonadWriter Any m)
  => TypeM s -> (MetaK s -> m ()) -> (MetaT s -> m ()) -> m (TypeM s)
zonkKindsAndTypesWith fs0 tweakKind tweakType = go fs0 where
  go fs = bindType id id <$> bitraverse handleKind handleType fs
  handleType m = do
    tweakType m
    zmv <- zonkWith (m^.metaValue) tweakKind
    readMeta m >>= \mv -> case mv of
      Nothing -> return $ return $ m & metaValue .~ zmv
      Just fmf -> do
        tell $ Any True
        r <- go fmf
        r <$ writeMeta m r
  handleKind m = do
    tweakKind m
    readMeta m >>= \mv -> case mv of
      Nothing -> return (return m)
      Just fmf -> do
        tell $ Any True
        r <- zonkWith fmf tweakKind
        r <$ writeMeta m r
{-# INLINE zonkKindsAndTypesWith #-}

-- | Unify two types, with access to a visited set, logging via 'MonadWriter' whether or not the answer differs from the first type argument.
--
-- This returns the result of unification with any modifications expanded, as we calculated it in passing
unifyType
  :: (MonadWriter Any m, MonadMeta s m)
  => TypeM s -> TypeM s -> m (TypeM s)
unifyType t1 t2 = do
  t1' <- semiprune t1
  t2' <- semiprune t2
  go t1' t2'
  where
    go x@(Var tv1)                (Var tv2)              | tv1 == tv2 = return x
    go x@(Var (Meta k _ i r d u)) y@(Var (Meta l _ j s e v)) = do
       () <$ unifyKind k l -- TODO: put the result in x/y?
       -- union-by-rank
       m <- liftST $ readSTRef u
       n <- liftST $ readSTRef v
       case compare m n of
         LT -> unifyTV True i r d y $ return ()
         EQ -> unifyTV True i r d y $ writeSTRef v $! n + 1
         GT -> unifyTV False j s e x $ return ()
    go (Var (Meta k _ i r d _)) t                       = do
      checkKind (view metaValue <$> t) k
      unifyTV True i r d t $ return ()
    go t                      (Var (Meta k _ i r d _))  = do
      checkKind (view metaValue <$> t) k
      unifyTV False i r d t $ return () -- not as boring as it could be
    go (App f x)              (App g y)               = App <$> unifyType f g <*> unifyType x y
    go (Loc l s)              t                       = Loc l <$> unifyType s t
    go s                      (Loc _ t)               = unifyType s t
    go Exists{}               _                       = fail "unifyType: existential"
    go _                      Exists{}                = fail "unifyType: existential"
    go t@(Forall m xs _ a)   t'@(Forall n ys _ b)
      | m /= n                 = fail "unifyType: forall: mismatched kind arity"
      | length xs /= length ys = fail "unifyType: forall: mismatched type arity"
      | otherwise = do
        ((sts, sks), Any modified) <- listen $ do
          sks <- for m $ newMeta False
          let nxs = instantiateVars sks <$> fmap extract xs
              nys = instantiateVars sks <$> fmap extract ys
          sts <- for (zip nxs nys) $ \(x,y) -> do
            k <- unifyKind x y
            newMeta k Nothing
          _ <- unifyType (instantiateKindVars sks (instantiateVars sts a))
                         (instantiateKindVars sks (instantiateVars sts b))
          return (sts, sks)
        -- checking skolem escapes here is important for cases like:
        --  ((f : some b r. (forall a. a -> b) -> r)
        --   (g : some a r. (forall b. a -> b) -> r) ->
        --     ((x y -> x) : some a. a -> a -> a) f g)
        if modified
         then do
           _ <- checkDistinct sks
           sts' <- checkDistinct sts
           fst <$> checkSkolems Nothing both sts' (t, t')
         else return t
    go t@(HardType x) (HardType y) | x == y = return t
    go _ _ = fail "type mismatch"
{-# INLINE unifyType #-}

-- | Unify with (the guts of) a type variable.
unifyTV
  :: (MonadWriter Any m, MonadMeta s m)
  => Bool -> Int -> STRef s (Maybe (TypeM s)) -> STRef s Depth
  -> TypeM s -> ST s () -> m (TypeM s)
unifyTV interesting i r d t bump = liftST (readSTRef r) >>= \ mt1 -> case mt1 of
  Just j -> do
    (t', Any m) <- listen $ unifyType j t
    if m then liftST $ t' <$ writeSTRef r (Just t')
         else j <$ tell (Any True)
  Nothing -> case t of
    Var v@(Meta k _ _ _ e _) -> do -- this has been semipruned so its not part of a chain
      tell (Any interesting)
      zk <- zonkWith k $ \kv -> liftST $ do
        let f = kv^.metaDepth
        depth1 <- readSTRef d
        depth2 <- readSTRef f
        when (depth2 > depth1) $ writeSTRef f depth1
      liftST $ do
        bump
        writeSTRef r (Just t)
        depth1 <- readSTRef d
        depth2 <- readSTRef e
        when (depth2 > depth1) $ writeSTRef e depth1
        return $ Var $ v & metaValue .~ zk
    _ -> do
      tell (Any interesting)
      depth1 <- liftST $ readSTRef d
      zt <- typeOccurs depth1 t $ \v -> v^.metaId == i
      zt <$ liftST (writeSTRef r (Just zt))
