{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Foldable as Foldable
import Data.List as List (partition)
import Data.Text as SText (pack)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Traversable
import Ermine.Builtin.Type as Type
import Ermine.Syntax
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
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Prelude hiding (foldr, any)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)
import Text.Trifecta.Result

type WitnessM s a = Witness (TypeM s) a

type PatM s = Pattern (Annot (MetaK s) (MetaT s))
type TermM s a = Term (Annot (MetaK s) (MetaT s)) a
type ScopeM b s a = Scope b (Term (Annot (MetaK s) (MetaT s))) a

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
          => (v -> TypeM s) -> TermM s v -> m (WitnessM s v)
inferType cxt (Term.Var v) = unfurl (cxt v) v
inferType _   (HardTerm t) = inferHardType t
inferType cxt (Loc r tm)   = localMeta (metaRendering .~ r) $ inferType cxt tm
inferType cxt (Remember i t) = do
  r <- inferType cxt t
  remember i (r^.witnessType)
  return r
inferType cxt (Sig tm (Annot ks ty)) = do
  ts <- for ks newMeta
  checkType cxt tm (instantiateVars ts ty)
inferType cxt (Term.App f x) = do
  Witness frcs ft fc <- inferType cxt f
  (i, o) <- matchFunType ft
  Witness xrcs _ xc <- checkType cxt x i
  simplifiedWitness (frcs ++ xrcs) o $ app # (fc, xc)
inferType cxt (Term.Lam ps e) = do
  (pts, ppts) <- unzip <$> traverse inferPatternType ps
  let pcxt (ArgPP i pp)
        | Just f <- ppts ^? ix (fromIntegral i) = f pp
      pcxt _ = error "panic: bad argument reference in lambda term"
  Witness rcs t c <- inferTypeInScope pcxt cxt e
  let cc = Pattern.compileLambda ps (splitScope c) (error "dummy PCompEnv" :: PCompEnv)
  return $ Witness rcs (foldr (~~>) t pts) (lambda (fromIntegral $ length ps) cc)

inferType _   (Term.Case _ _) = fail "unimplemented"
inferType _   (Term.Let _ _)  = fail "unimplemented"

inferTypeInScope :: MonadDischarge s m
                 => (b -> TypeM s) -> (v -> TypeM s)
                 -> ScopeM b s v -> m (WitnessM s (Var b v))
inferTypeInScope aug cxt sm = inferType (unvar aug cxt) $ fromScope sm

-- TODO: write this
simplifiedWitness :: MonadDischarge s m => [TypeM s] -> TypeM s -> CoreM s a -> m (WitnessM s a)
simplifiedWitness rcs t c = Witness rcs t <$> simplifyVia (toList c) c

checkType :: MonadMeta s m
          => (v -> TypeM s) -> TermM s a -> TypeM s -> m (WitnessM s a)
checkType _   _ _ = fail "Check yourself"

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
  t <- runSharing t0 $ zonk t0
  let tvks = toListOf typeVars t <&> \v -> (v, v^.metaValue)
      kvs = toListOf kindVars t ++ toListOf (traverse._2.kindVars) tvks
      bad :: Meta s f a -> Bool
      bad v = has skolem v && not (sks^.contains (v^.metaId))
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

inferPatternType :: MonadMeta s m =>  PatM s -> m (TypeM s, PatPath -> TypeM s)
inferPatternType (SigP ann)  = instantiateAnnot ann <&> \ty ->
  (ty, \case LeafPP -> ty ; _ -> error "panic: bad pattern path")
inferPatternType WildcardP   =
  pure <$> newMeta star <&> \m ->
    (m, \case LeafPP -> m ; _ -> error "panic: bad pattern path")
inferPatternType (AsP p)     =
  inferPatternType p <&> \(ty, pcxt) ->
    (ty, \case LeafPP -> ty ; pp -> pcxt pp)
inferPatternType (StrictP p) = inferPatternType p
inferPatternType (LazyP p)   = inferPatternType p
inferPatternType (TupP ps)   =
  unzip <$> traverse inferPatternType ps <&> \(tys, cxts) ->
    ( apps (tuple $ length ps) tys
    , \case FieldPP i pp | Just f <- cxts ^? ix (fromIntegral i) -> f pp
            _ -> error "panic: bad pattern path"
    )
inferPatternType (LitP l)    =
  pure (literalType l, \_ -> error "panic: bad pattern path")
inferPatternType (ConP _ _)  = error "unimplemented"

instantiateAnnot :: MonadMeta s m => Annot (MetaK s) (MetaT s) -> m (TypeM s)
instantiateAnnot (Annot ks sc) = do
  traverse (fmap pure . newMeta) ks <&> \tvs ->
    instantiate (tvs !!) sc
