{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Inference.Type
  ( matchFunType
  , inferType
  , inferPatternType
  , generalizeType
  , refreshType
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.Bitraversable
import Data.Foldable as Foldable
import Data.List as List (partition)
import Data.Text as SText (pack)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set.Lens
import Data.STRef
import Data.Traversable
import Ermine.Builtin.Type as Type
import Ermine.Syntax
import Ermine.Syntax.Name
import Ermine.Syntax.Hint
import Ermine.Syntax.Literal
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Pattern as Pattern
import Ermine.Syntax.Pattern.Compiler as Pattern
import Ermine.Syntax.Scope
import Ermine.Syntax.Term as Term
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var, Loc)
import Ermine.Inference.Discharge
import Ermine.Inference.Kind
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Prelude hiding (foldr, any, concat)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)
import Text.Trifecta.Result

type WitnessM s a = Witness (TypeM s) a

type AnnotM s = Annot (MetaK s) (MetaT s)
type PatM s = Pattern (AnnotM s)
type TermM s a = Term (AnnotM s) a
type AltM s a = Alt (AnnotM s) (Term (AnnotM s)) a
type ScopeM b s a = Scope b (Term (AnnotM s)) a

type CoreM s a = Scope a Core (TypeM s)

matchFunType :: MonadMeta s m => TypeM s -> m (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ unsharingT (unifyType t (x ~> y))

-- discharge :: [TypeM s] -> TypeM s -> M s (Maybe ([TypeM s], Core (Var (Either Int Int) Id)

inferType :: MonadDischarge s m
          => Depth -> (v -> TypeM s) -> TermM s v -> m (WitnessM s v)
inferType _ cxt (Term.Var v) = unfurl (cxt v) v
inferType _ _   (HardTerm t) = inferHardType t
inferType d cxt (Loc r tm)   = localMeta (metaRendering .~ r) $ inferType d cxt tm
inferType d cxt (Remember i t) = do
  r <- inferType d cxt t
  remember i (r^.witnessType)
  return r
inferType d cxt (Sig tm (Annot ks ty)) = do
  ts <- for ks newMeta
  checkType d cxt tm (instantiateVars ts ty)
inferType d cxt (Term.App f x) = do
  Witness frcs ft fc <- inferType d cxt f
  (i, o) <- matchFunType ft
  Witness xrcs _ xc <- checkType d cxt x i
  simplifiedWitness (frcs ++ xrcs) o $ app # (fc, xc)
inferType d cxt (Term.Lam ps e) = do
  (skss, pts, ppts) <- unzip3 <$> traverse (inferPatternType d) ps
  let pcxt (ArgPP i pp)
        | Just f <- ppts ^? ix (fromIntegral i) = f pp
      pcxt _ = error "panic: bad argument reference in lambda term"
  Witness rcs t c <- inferTypeInScope (d+1) pcxt cxt e
  let cc = Pattern.compileLambda ps (splitScope c) dummyPCompEnv
  rt <- checkSkolemEscapes d id (join skss) $ foldr (~~>) t pts
  return $ Witness rcs rt (lambda (fromIntegral $ length ps) cc)

inferType d cxt (Term.Case e b) = do
  w <- inferType d cxt e
  inferAltTypes d cxt w b
inferType _ _   (Term.Let _ _)  = fail "unimplemented"

inferTypeInScope :: MonadDischarge s m
                 => Depth -> (b -> TypeM s) -> (v -> TypeM s)
                 -> ScopeM b s v -> m (WitnessM s (Var b v))
inferTypeInScope d aug cxt sm = inferType d (unvar aug cxt) $ fromScope sm

inferAltTypes :: MonadDischarge s m
              => Depth -> (v -> TypeM s) -> WitnessM s v
              -> [AltM s v]
              -> m (WitnessM s v)
inferAltTypes d cxt (Witness r t c) bs = do
  (ts, gws) <- unzip <$> traverse inferAlt bs
  let ps = map (\(Alt p _) -> p) bs
      rs = gws >>= (>>= view witnessRowConstraints . snd)
      bts = gws >>= fmap (view witnessType . snd)
      gcs = over (traverse.traverse._2) (splitScope . view witnessCore) gws
      c' = Pattern.compileCase ps c gcs dummyPCompEnv
  runSharing t $ foldlM unifyType t ts
  result <- pure <$> newMeta star
  t' <- runSharing result $ foldlM unifyType result bts
  return $ Witness (r++rs) t' c'
 where
 inferAlt (Alt p b) = do
   (sks, pt, pcxt) <- inferPatternType d p
   bgws <- inferGuarded pcxt b
   checkSkolemEscapes d (beside id $ traverse._2.witnessType) sks (pt, bgws)

 inferGuarded pcxt (Unguarded s) =
   return . (Trivial,) <$> inferTypeInScope (d+1) pcxt cxt s
 inferGuarded pcxt (Guarded gbs) =
   for gbs $ \(g, b) -> do
     Witness _ _ gc <- checkTypeInScope (d+1) pcxt cxt g Type.bool
     w <- inferTypeInScope (d+1) pcxt cxt b
     return (Pattern.Explicit $ splitScope gc, w)

checkSkolemEscapes :: MonadMeta s m
                   => Depth -> LensLike' m ts (TypeM s) -> [MetaT s] -> ts -> m ts
checkSkolemEscapes d trav sks ts = do
  for_ sks $ \s ->
    liftST (readSTRef $ s^.metaDepth) >>= \d' -> when (d' < d) serr
  trav (\t -> runSharing t $ zonkWith t tweak) ts
 where
 serr = fail "escaped skolem"
 skids = setOf (traverse.metaId) sks
 tweak v = when (has (ix $ v^.metaId) skids) $ lift serr

-- TODO: write this
simplifiedWitness :: MonadDischarge s m => [TypeM s] -> TypeM s -> CoreM s a -> m (WitnessM s a)
simplifiedWitness rcs t c = Witness rcs t <$> simplifyVia (toList c) c

-- TODO: write this correctly
checkType :: MonadDischarge s m
          => Depth -> (v -> TypeM s) -> TermM s v -> TypeM s -> m (WitnessM s v)
checkType d cxt e t = do
  w <- inferType d cxt e
  checkKind (fmap (view metaValue) t) star
  subsumesType d w t

checkTypeInScope :: MonadDischarge s m
                 => Depth -> (b -> TypeM s) -> (v -> TypeM s)
                 -> ScopeM b s v -> TypeM s -> m (WitnessM s (Var b v))
checkTypeInScope d aug cxt e t = checkType d (unvar aug cxt) (fromScope e) t

subsumesType :: MonadDischarge s m
             => Depth -> WitnessM s v -> TypeM s -> m (WitnessM s v)
subsumesType d (Witness rs t1 c) t2 = do
  t1' <- refreshType d t1
  -- TODO: constraints
  (sks, sts, _, t2') <- skolemize d t2
  runSharing t1' $ unifyType t1' t2'
  -- TODO: skolem kinds
  checkSkolemEscapes d (beside id id) sts (t1,t2) <&> \(_, t2'') ->
  -- TODO: fix up core representation
    Witness rs t2'' c

refreshVar :: (MonadMeta s m, MonadWriter Any m)
           => Depth -> Meta s f a -> m (Bool, Meta s f a)
refreshVar d m = do
  d' <- liftST . readSTRef $ m^.metaDepth
  if d' >= d
    then tell (Any True) >> (,) True <$> newMeta (m^.metaValue)
    else return (True, m)

refreshType :: MonadMeta s m => Depth -> TypeM s -> m (TypeM s)
refreshType d t0 = do
  t <- runSharing t0 $ zonk t0 >>= zonkKinds
  runSharing t $ memoverse (refreshVar d) (refreshVar d) t

-- | Generalizes the metavariables in a TypeM, yielding a type with no free
-- variables. Checks for skolem escapes while doing so.
--
-- TODO: constraint contexts
generalizeType :: MonadMeta s m => TypeM s -> m (Type k t)
generalizeType = generalizeOverType IntSet.empty

-- TODO: hints
-- TODO: separate kind vs type skolem sets?
generalizeOverType :: MonadMeta s m => IntSet -> TypeM s -> m (Type k t)
generalizeOverType sks t0 = do
  t <- runSharing t0 $ zonk t0 >>= zonkKinds
  let tvks = toListOf typeVars t <&> \v -> (v, v^.metaValue)
      kvs = toListOf kindVars t ++ toListOf (traverse._2.kindVars) tvks
      bad :: Meta s f a -> Bool
      bad v = has _Skolem v && not (sks^.contains (v^.metaId))
  when (any (bad.fst) tvks || any bad kvs) $
    fail "escaped skolem"
  case closedType $ forall (const noHint) (const noHint) kvs tvks (And []) t of
    Just ty -> pure ty
    Nothing -> error "panic: generalizeOverType failed to generalize all variables"

inferHardType :: MonadMeta s m => HardTerm -> m (WitnessM s a)
inferHardType (Term.Lit l) = return $ Witness [] (literalType l) (_Lit # l)
inferHardType (Term.Tuple n) = do
  vs <- replicateM (fromIntegral n) $ pure <$> newMeta star
  return $ Witness [] (foldr (~>) (tup vs) vs) $ dataCon n 0
inferHardType Hole = do
  tv <- newMeta star
  r <- viewMeta metaRendering
  return $ Witness [] (Type.Var tv) $ _HardCore # (Core.Error $ SText.pack $ show $ plain $ explain r $ Err (Just (text "open hole")) [] mempty)
inferHardType _ = fail "Unimplemented"

literalType :: Literal -> Type k a
literalType Int{}    = Type.int
literalType Long{}   = long
literalType Byte{}   = byte
literalType Short{}  = short
literalType String{} = Type.string
literalType Char{}   = Type.char
literalType Float{}  = Type.float
literalType Double{} = Type.double

skolemize :: MonadMeta s m
          => Depth -> TypeM s -> m ([MetaK s], [MetaT s], Maybe (TypeM s), TypeM s)
skolemize d (Forall ks ts cs bd) = do
  sks <- for ks $ newSkolem . extract
  sts <- for ts $ newSkolem . instantiateVars sks . extract
  let inst = instantiateKindVars sks . instantiateVars sts
  return (sks, sts, Just (inst cs), inst bd)
skolemize _ t = return ([], [], Nothing, t)

unfurl :: MonadMeta s m => TypeM s -> a -> m (WitnessM s a)
unfurl (Forall ks ts cs bd) co = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  let inst = instantiateKindVars mks . instantiateVars mts
  (rcs, tcs) <- unfurlConstraints . inst $ cs
  return $ Witness rcs (inst bd) $ appDicts (Scope . pure $ B co) (pure <$> tcs)
unfurl t co = pure $ Witness [] t (Scope . pure $ B co)


partConstraints :: TypeM s -> ([TypeM s], [TypeM s])
partConstraints (And l) = List.partition isRowConstraint l
partConstraints c | isRowConstraint c = ([c], [])
                  | otherwise         = ([], [c])

unfurlConstraints :: MonadMeta s m => TypeM s -> m ([TypeM s], [TypeM s])
unfurlConstraints (Exists ks ts cs) = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  pure . partConstraints . instantiateKindVars mks . instantiateVars mts $ cs
unfurlConstraints c = pure $ partConstraints c

inferPatternType :: MonadMeta s m =>  Depth -> PatM s
                 -> m ([MetaT s], TypeM s, PatPath -> TypeM s)
inferPatternType d (SigP ann)  = do
  ty <- instantiateAnnot d ann
  checkKind (view metaValue <$> ty) star
  return ([], ty, \case LeafPP -> ty ; _ -> error "panic: bad pattern path")
inferPatternType d WildcardP   =
  pure <$> newShallowMeta d star <&> \m ->
    ([], m, \case LeafPP -> m ; _ -> error "panic: bad pattern path")
inferPatternType d (AsP p)     =
  inferPatternType d p <&> \(sks, ty, pcxt) ->
    (sks, ty, \case LeafPP -> ty ; pp -> pcxt pp)
inferPatternType d (StrictP p) = inferPatternType d p
inferPatternType d (LazyP p)   = inferPatternType d p
inferPatternType d (TupP ps)   =
  unzip3 <$> traverse (inferPatternType d) ps <&> \(sks, tys, cxts) ->
    ( join sks
    , apps (tuple $ length ps) tys
    , \case FieldPP i pp | Just f <- cxts ^? ix (fromIntegral i) -> f pp
            _ -> error "panic: bad pattern path"
    )
inferPatternType _ (LitP l)    =
  pure ([], literalType l, \_ -> error "panic: bad pattern path")
-- Hard coding temporarily to test.
--
-- data E = forall x. E x
-- E : forall (x : *). x -> E
inferPatternType d (ConP g ps)
  | g^.name == "E" =
    newShallowSkolem d star >>= \x ->
    case ps of
      [p] -> do (sks, ty, f) <- inferPatternType d p
                runSharing () $ () <$ unifyType (pure x) ty
                return ( x:sks
                       , builtin_ "E"
                       , \case FieldPP 0 pp -> f pp
                               _ -> error "panic: bad pattern path"
                       )
      _ -> fail "over-applied constructor"
inferPatternType _ (ConP _ _)  = error "unimplemented"

instantiateAnnot :: MonadMeta s m => Depth -> Annot (MetaK s) (MetaT s) -> m (TypeM s)
instantiateAnnot d (Annot ks sc) = do
  traverse (fmap pure . newShallowMeta d) ks <&> \tvs ->
    instantiate (tvs !!) sc
