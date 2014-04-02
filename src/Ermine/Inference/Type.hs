{-# LANGUAGE CPP #-}
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
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2011-2014
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
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.ST.Class
import Control.Monad.Writer.Strict
import Data.Foldable as Foldable
import Data.Graph
import qualified Data.Map as Map
import Data.List as List (partition, nub, group, elemIndex)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set.Lens
import Data.STRef
import Data.Text as SText (pack)
import Data.Void
import Data.Word
import Data.Traversable
import Ermine.Builtin.Type as Type
import Ermine.Constraint.Env
import Ermine.Constraint.Simplification
import Ermine.Pattern.Env as Pattern
import Ermine.Pattern.Matching as Pattern
import Ermine.Syntax
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Global
import Ermine.Syntax.Name
import Ermine.Syntax.Hint
import Ermine.Syntax.Literal
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Pattern as Pattern
import Ermine.Syntax.Scope
import Ermine.Syntax.Term as Term
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var, Loc)
import Ermine.Inference.Kind
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Prelude hiding (foldr, any, concat)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty, (<>))
import Text.Trifecta.Result

type WitnessM s a = Witness (TypeM s) a
type AnnotM s = Annot (MetaK s) (MetaT s)
type PatM s = Pattern (AnnotM s)
type TermM s a = Term (AnnotM s) a
type AltM s a = Alt (AnnotM s) (Term (AnnotM s)) a
type ScopeM b s a = Scope b (Term (AnnotM s)) a
type BindingM s a = Binding (AnnotM s) a
type CoreM s a = Scope a Core (TypeM s)

matchFunType :: MonadMeta s m => TypeM s -> m (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ unsharingT (unifyType t (x ~> y))

type WMap = Map.Map Word32

beside3 :: Applicative f => LensLike f s1 t1 a b -> LensLike f s2 t2 a b -> LensLike f s3 t3 a b -> LensLike f (s1,s2,s3) (t1,t2,t3) a b
beside3 x y z f (a,b,c) = (,,) <$> x f a <*> y f b <*> z f c

inferBindingType
  :: MonadConstraint s m
  => Depth -> (v -> TypeM s) -> WMap (TypeM s) -> BindingM s v -> m (WitnessM s (Var Word32 v))
inferBindingType d cxt lcxt bdg = do
  let ps = bdg^..bindingBodies.traverse.bodyPatterns
  case compare (length (List.group $ map length ps)) 1 of
    LT -> fail "panic: missing right hand sides"
    EQ -> return ()
    GT -> fail "pattern arity mismatch"
  mess <- for (bdg^.bindingBodies) $ \(Body bps gd wc) -> do
    (skss, pts, ppts) <- unzip3 <$> traverse (inferPatternType d) bps
    let pcxt (ArgPP i pp)
          | Just f <- ppts ^? ix (fromIntegral i) = f pp
        pcxt _ = error "panic: bad argument reference in lambda term"
        whereCxt (F a)             = cxt a
        whereCxt (B (WhereDecl w)) = lcxt^?!ix w
        whereCxt (B (WherePat p))  = pcxt p
    whereWitnesses <- inferBindings (d+1) whereCxt wc
    let wrs = whereWitnesses^..folded.witnessRowConstraints.folded
        bodyCxt (BodyDecl w)  = lcxt^?!ix w
        bodyCxt (BodyPat p)   = pcxt p
        bodyCxt (BodyWhere w) = whereWitnesses^?!ix (fromIntegral w).witnessType
    gd' <- for gd $ inferTypeInScope (d+1) bodyCxt cxt
    (rs, t, cores) <- coalesceGuarded gd'
    (rs', t', cores') <- checkSkolemEscapes (Just d) (beside3 traverse id $ traverse.traverse) (join skss) (rs ++ wrs, foldr (~~>) t pts, cores)
    let shuffle (B a)     = B (F a)
        shuffle (F (B a)) = B (B a)
        shuffle (F (F a)) = F a
    return (rs', t', splitScope <$> cores', whereWitnesses^..folded.witnessCore.to (splitScope . mapBound shuffle))

  -- if only someone could write unzip4!
  let rs       = mess^.folded._1
      guards   = view _3 <$> mess
      wheres   = view _4 <$> mess
      (t0:tys) = view _2 <$> mess
  ty <- runSharing t0 $ foldM unifyType t0 tys
  simplifiedWitness rs ty $ mergeScope $ compileBinding ps guards wheres dummyPatternEnv

inferBindingGroupTypes
  :: MonadConstraint s m
  => Depth -> (v -> TypeM s) -> WMap (TypeM s) -> WMap (BindingM s v) -> m (WMap (WitnessM s (Var Word32 v)))
inferBindingGroupTypes d cxt lcxt bg = do
  bgts <- for bg $ \ b -> instantiateAnnot d $ fromMaybe anyType $ b^?bindingType._Explicit
  witnesses <- for bg $ inferBindingType (d+1) cxt (bgts <> lcxt)
  sequenceA $ Map.intersectionWith ?? bgts ?? witnesses $ \vt w -> do
    w' <- subsumesType d w vt
    generalizeWitnessType d w'

inferBindings
  :: MonadConstraint s m
  => Depth -> (v -> TypeM s) -> [BindingM s v] -> m [WitnessM s (Var Word32 v)]
inferBindings d cxt bgs = do
  (ws,_ts) <- foldlM step (Map.empty, Map.empty) sccs
  return $ toList ws
 where
  is = zip [0..] bgs
  sccs = Map.fromList . flattenSCC <$> stronglyConnComp (comp <$> is)
  comp ib = (ib, fst ib, ib^.._2.bindingBodies.traverse.bodyDecls)
  step (ws,ts) cc = do
    nws <- inferBindingGroupTypes (d+1) cxt ts cc
    return (ws <> nws, ts <> fmap (view witnessType) nws)

inferType :: MonadConstraint s m
          => Depth -> (v -> TypeM s) -> TermM s v -> m (WitnessM s v)
inferType _ cxt (Term.Var v) = unfurl_ (cxt v) v
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
  simplifiedWitness (frcs ++ xrcs) o $ _App # (fc, xc)
inferType d cxt (Term.Lam ps e) = do
  (skss, pts, ppts) <- unzip3 <$> traverse (inferPatternType d) ps
  let pcxt (ArgPP i pp)
        | Just f <- ppts ^? ix (fromIntegral i) = f pp
      pcxt _ = error "panic: bad argument reference in lambda term"
  Witness rcs t c <- inferTypeInScope (d+1) pcxt cxt e
  let cc = Pattern.compileLambda ps (splitScope c) dummyPatternEnv
  rt <- checkSkolemEscapes (Just d) id (join skss) $ foldr (~~>) t pts
  return $ Witness rcs rt (lambda C (fromIntegral $ length ps) cc)

inferType d cxt (Term.Case e b) = do
  w <- inferType d cxt e
  inferAltTypes d cxt w b
inferType d cxt (Term.Let xs b) = do
  ws <- inferBindings d cxt xs
  let ts = map (view witnessType) ws
  w <- inferTypeInScope d (\i -> ts !! fromIntegral i) cxt b
  let wc  = w^.witnessCore.to splitScope
  let wsc = ws^..traverse.witnessCore.to splitScope
  simplifiedWitness (ws^.traverse.witnessRowConstraints ++ w^.witnessRowConstraints)
                    (w^.witnessType)
                  $ letrec wsc wc

inferTypeInScope :: MonadConstraint s m
                 => Depth -> (b -> TypeM s) -> (v -> TypeM s)
                 -> ScopeM b s v -> m (WitnessM s (Var b v))
inferTypeInScope d aug cxt sm = inferType d (unvar aug cxt) $ fromScope sm

inferAltTypes :: MonadConstraint s m
              => Depth -> (v -> TypeM s) -> WitnessM s v
              -> [AltM s v]
              -> m (WitnessM s v)
inferAltTypes d cxt (Witness r t c) bs = do
  (pts, l) <- unzip <$> traverse inferAlt bs
  let (rss, bts, gcs) = unzip3 l
      ps = map (\(Alt p _) -> p) bs
      rs = join rss
      c' = Pattern.compileCase ps c gcs dummyPatternEnv
  uncaring $ foldlM unifyType t pts
  result <- pure <$> newMeta star
  t' <- runSharing result $ foldlM unifyType result bts
  return $ Witness (r++rs) t' c'
 where
 inferAlt (Alt p b) = do
   (sks, pt, pcxt) <- inferPatternType d p
   let trav f (x, y, z) = (,,) <$> traverse f x <*> f y <*> (traverse.traverse) f z
   trip <- inferGuarded pcxt b
   checkSkolemEscapes (Just d) (beside id trav) sks (pt, trip)

 inferGuarded pcxt gb = do
   bgws <- traverse (inferTypeInScope (d+1) pcxt cxt) gb
   over _3 (splitScope <$>) <$> coalesceGuarded bgws

coalesceGuarded :: MonadMeta s m => Guarded (WitnessM s v) -> m ([TypeM s], TypeM s, Guarded (Scope v Core (TypeM s)))
coalesceGuarded (Unguarded (Witness r' t' c')) = pure (r', t', Unguarded c')
coalesceGuarded (Guarded l) = do
  rt <- pure <$> newMeta star
  (rs, ty, l') <- foldrM ?? ([], rt, []) ?? l $
    \(Witness gr gt gc, Witness br bt bc) (rs, ty, gcs) -> do
      uncaring $ unifyType gt Type.bool
      (rs++gr++br,,(gc,bc):gcs) <$> runSharing ty (unifyType ty bt)
  pure (rs, ty, Guarded l')

-- TODO: write this
simplifiedWitness :: MonadConstraint s m => [TypeM s] -> TypeM s -> CoreM s a -> m (WitnessM s a)
simplifiedWitness rcs t c = runSharing c (traverse zonk c) >>= \c' ->
    Witness rcs t <$> simplifyVia (toList c') c'

abstractedWitness :: MonadConstraint s m
                  => Int -> [MetaT s] -> [TypeM s] -> [TypeM s] -> TypeM s -> CoreM s a
                  -> m (WitnessM s a)
abstractedWitness d sks rs cs0 ty co0 = do
  cs <- runSharing cs0 $ traverse zonk cs0
  co <- runSharing co0 $ traverse zonk co0
  co' <- simplifyVia cs co
  ((rs', ty'), co'') <-
    checkSkolemEscapes (Just d) (traverse`beside`id`beside`traverse) sks
      ((rs, ty), lambda D (fromIntegral $ length cs) $ abstract (fmap fromIntegral . flip elemIndex cs) co')
  pure $ Witness rs' ty' co''

-- TODO: write this correctly
checkType :: MonadConstraint s m
          => Depth -> (v -> TypeM s) -> TermM s v -> TypeM s -> m (WitnessM s v)
checkType d cxt e t = do
  w <- inferType d cxt e
  checkKind (view metaValue <$> t) star
  subsumesType d w t

subsumesType :: MonadConstraint s m
             => Depth -> WitnessM s v -> TypeM s -> m (WitnessM s v)
subsumesType d (Witness rs t1 c) t2 = do
  (_  , sts, cs, t2') <- skolemize d t2
  uncaring $ unifyType t1 t2'
  -- TODO: skolem kinds
  abstractedWitness d sts rs cs t2 c

cabs :: Eq a => [a] -> Var b a -> Maybe Word8
cabs cls (F ty) = fromIntegral <$> elemIndex ty cls
cabs _   _      = Nothing

-- | Generalizes the metavariables in a TypeM, yielding a type with no free
-- variables. Checks for skolem escapes while doing so.
--
-- TODO: constraint contexts
generalizeWitnessType
  :: MonadMeta s m
  => Depth -> WitnessM s v -> m (WitnessM s v)
generalizeWitnessType min_d (Witness r0 t0 c0) = do
  r <- runSharing r0 $ traverse zonkKindsAndTypes r0
  t <- runSharing t0 $ zonkKindsAndTypes t0
  c <- runSharing c0 $ traverse zonkKindsAndTypes c0
  let cc = nub (toList c)

  tvs <- filterM ?? toListOf typeVars (t,cc,r) $ \ tv -> do
    d <- liftST $ readSTRef (tv^.metaDepth)
    return (min_d <= d)

  let s = setOf traverse tvs
      cc' = filter (any (`Set.member` s)) cc

  let kvs0 = toListOf kindVars t ++ toListOf (traverse.metaValue.kindVars) tvs
  kvs <- filterM ?? kvs0 $ \kv -> do
    d <- liftST $ readSTRef (kv^.metaDepth)
    return (min_d <= d)

  let (r',cc'')
        | min_d == 0 = ([],r ++ cc')
        | otherwise  = (r, cc')

  when (any (any (`Set.member` s)) r') $
    fail "Unable to abstract over a local row constraint"

  let n = length cc'

  return $ Witness r'
    (forall
        (const noHint)
        (const noHint)
        kvs
        (tvs <&> \tv -> (tv, tv^.metaValue))
        (allConstraints cc'')
        t)
    $ if n == 0
      then c
      else toScope $ Core.Lam D (fromIntegral $ length cc') $ abstract (cabs cc') $ fromScope c

generalizeType :: MonadMeta s m => WitnessM s a -> m (Type k t, Core a)
generalizeType w = do
  Witness [] t c <- generalizeWitnessType 0 w
  c' <- traverse (unvar pure $ \_ -> fail "panic: generalizeType failed to abstract over all class constraints") (fromScope c)
  case closedType t of
    Just t' -> pure (t', c')
    Nothing -> fail "panic: generalizeType failed to generalize all variables"

inferHardType :: MonadMeta s m => HardTerm -> m (WitnessM s a)
inferHardType (Term.Lit l) = return $ Witness [] (literalType l) (_Lit # l)
inferHardType (Term.Tuple n) = do
  vs <- replicateM (fromIntegral n) $ pure <$> newMeta star
  return $ Witness [] (foldr (~>) (tup vs) vs) $ dataCon n 0 (tupleg n)
inferHardType Hole = do
  tv <- newMeta star
  r <- viewMeta metaRendering
  return $ Witness [] (Type.Var tv) $ _HardCore # (Core.Error $ SText.pack $ show $ plain $ explain r $ Err (Just (text "open hole")) [] mempty)
inferHardType (DataCon g t)
  | g^.name == "Nothing" = unfurl (bimap absurd absurd t) $ dataCon 0 0 g
  | g^.name == "Just"    = unfurl (bimap absurd absurd t) $ dataCon 1 1 g
  | g^.name == "E"       = unfurl (bimap absurd absurd t) $ dataCon 1 0 g
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
          => Depth -> TypeM s -> m ([MetaK s], [MetaT s], [TypeM s], TypeM s)
skolemize _ (Forall ks ts cs bd) = do
  sks <- for ks $ newSkolem . extract
  sts <- for ts $ newSkolem . instantiateVars sks . extract
  let inst = instantiateKindVars sks . instantiateVars sts
  (rs, tcs) <- unfurlConstraints $ inst cs
  unless (null rs) $
    fail "invalid higher-rank row constraints"
  return (sks, sts, tcs, inst bd)
skolemize _ t = return ([], [], [], t)

unfurl :: MonadMeta s m => TypeM s -> Scope a Core (TypeM s) -> m (WitnessM s a)
unfurl (Forall ks ts cs bd) co = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  let inst = instantiateKindVars mks . instantiateVars mts
  (rcs, tcs) <- unfurlConstraints . inst $ cs
  return $ Witness rcs (inst bd) $ appDicts co (pure <$> tcs)
unfurl t co = pure $ Witness [] t co

unfurl_ :: MonadMeta s m => TypeM s -> a -> m (WitnessM s a)
unfurl_ ty v = unfurl ty . Scope . pure . B $ v

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
                 -> m ([MetaT s], TypeM s, PatternPath -> TypeM s)
inferPatternType d (SigP ann)  = do
  ty <- instantiateAnnot d ann
  checkKind (view metaValue <$> ty) star
  return ([], ty, \ xs -> case xs of LeafPP -> ty ; _ -> error "panic: bad pattern path")
inferPatternType d WildcardP   =
  pure <$> newShallowMeta d star <&> \m ->
    ([], m, \ xs -> case xs of LeafPP -> m ; _ -> error "panic: bad pattern path")
inferPatternType d (AsP p)     =
  inferPatternType d p <&> \(sks, ty, pcxt) ->
    (sks, ty, \ xs -> case xs of LeafPP -> ty ; pp -> pcxt pp)
inferPatternType d (StrictP p) = inferPatternType d p
inferPatternType d (LazyP p)   = inferPatternType d p
inferPatternType d (TupP ps)   =
  unzip3 <$> traverse (inferPatternType d) ps <&> \(sks, tys, cxts) ->
    ( join sks
    , apps (tuple $ length ps) tys
    , \ xs -> case xs of
       FieldPP i pp        | Just f <- cxts ^? ix (fromIntegral i) -> f pp
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
      [ ] -> fail "under-applied constructor"
      [p] -> do (sks, ty, f) <- inferPatternType d p
                uncaring $ unifyType (pure x) ty
                return ( x:sks
                       , ee
                       , \xs -> case xs of
                         FieldPP 0 pp -> f pp
                         _ -> error "panic: bad pattern path"
                       )
      _ -> fail "over-applied constructor"
  | g^.name == "Just" =
    case ps of
      [ ] -> fail "under-applied constructor"
      [p] -> do (sks, ty, f) <- inferPatternType d p
                return (sks, maybe_ ty, \xs -> case xs of
                          FieldPP 0 pp -> f pp
                          _ -> error "panic: bad pattern path")
      _ -> fail "over-applied constructor"
  | g^.name == "Nothing" =
    case ps of
      [] -> newShallowMeta d star >>= \x ->
              return ([], maybe_ $ pure x, error "panic: bad pattern path")
      _ -> fail "over-applied constructor"
inferPatternType _ c@(ConP _ _)  = error $ "inferPatternType: constructor unimplemented: " ++ show c

instantiateAnnot :: MonadMeta s m => Depth -> Annot (MetaK s) (MetaT s) -> m (TypeM s)
instantiateAnnot d (Annot ks sc) = do
  ty <- traverse (fmap pure . newShallowMeta d) ks <&> \tvs ->
    instantiate (tvs !!) sc
  ty <$ checkKind (view metaValue <$> ty) star
