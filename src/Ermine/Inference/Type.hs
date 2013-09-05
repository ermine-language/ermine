{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
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
  , unfurl
  , partConstraints
  , unfurlConstraints
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
import Data.Traversable
import Ermine.Builtin.Pattern as Pattern
import Ermine.Builtin.Type as Type
import Ermine.Syntax
import Ermine.Syntax.Id
import Ermine.Syntax.Literal
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Pattern as Pattern
import Ermine.Syntax.Scope
import Ermine.Syntax.Term as Term
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var, Loc)
import Ermine.Inference.Discharge
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)
import Text.Trifecta.Result

type WitnessM s a = Witness (TypeM s) a

type TermM s = Term (Annot (MetaK s) (MetaT s)) (TypeM s)

type CoreM s a = Scope a Core (TypeM s)

matchFunType :: MonadMeta s m => TypeM s -> m (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ unsharingT (unifyType t (x ~> y))

-- discharge :: [TypeM s] -> TypeM s -> M s (Maybe ([TypeM s], Core (Var (Either Int Int) Id)

inferType :: MonadDischarge s m => TermM s -> m (WitnessM s a)
inferType (Term.Var _) = fail "unimplemented"
inferType (HardTerm t) = inferHardType t
inferType (Loc r tm)   = localMeta (metaRendering .~ r) $ inferType tm
inferType (Remember i t) = do
  r <- inferType t
  remember i (r^.witnessType)
  return r
inferType (Sig tm (Annot ks ty)) = do
  ts <- for ks newMeta
  checkType tm (instantiateVars ts ty)
inferType (Term.App f x) = do
  Witness frcs ft fc <- inferType f
  (i, o) <- matchFunType ft
  Witness xrcs _ xc <- checkType x i
  simplifiedWitness (frcs ++ xrcs) o $ app # (fc, xc)
inferType (Term.Lam _ _)  = fail "unimplemented"
inferType (Term.Case _ _) = fail "unimplemented"
inferType (Term.Let _ _)  = fail "unimplemented"

-- TODO: write this
simplifiedWitness :: MonadDischarge s m => [TypeM s] -> TypeM s -> CoreM s a -> m (WitnessM s a)
simplifiedWitness rcs t c = Witness rcs t <$> simplifyVia (toListOf (traverse . _F) c) c

checkType :: MonadMeta s m => TermM s -> TypeM s -> m (WitnessM s a)
checkType _ _ = fail "Check yourself"

inferHardType :: MonadMeta s m => HardTerm -> m (WitnessM s a)
inferHardType (Term.Lit l) = return $ Witness [] (literalType l) (HardCore (Core.Lit l))
inferHardType (Term.Tuple n) = do
  vars <- replicateM (fromIntegral n) $ pure <$> newMeta star
  return $ Witness [] (Prelude.foldr (~>) (tup vars) vars) $ dataCon n 0
inferHardType Hole = do
  tv <- newMeta star
  r <- viewMeta metaRendering
  return $ Witness [] (Type.Var tv) $ HardCore $ Core.Error $ SText.pack $ show $ plain $ explain r $ Err (Just (text "open hole")) [] mempty
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

unfurl :: MonadMeta s m => TypeM s -> Core a -> m (WitnessM s a)
unfurl (Forall ks ts cs bd) co = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  let inst = instantiateKindVars mks . instantiateVars mts
  (rcs, tcs) <- unfurlConstraints . inst $ cs
  return $ Witness rcs (inst bd) $ Foldable.foldl AppDict (B <$> co) (pure . F <$> tcs)
unfurl t co = pure $ Witness [] t (B <$> co)

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

inferPatternType :: MonadMeta s m => Pattern (TypeM s) -> m (Binder (TypeM s) (TypeM s))
inferPatternType (SigP t)    = pure $ noted t
inferPatternType WildcardP   = noted . pure <$> newMeta star
inferPatternType (AsP p)     = note <$> inferPatternType p
inferPatternType (StrictP p) = inferPatternType p
inferPatternType (LazyP p)   = inferPatternType p
inferPatternType (TupP ps)   = fmap (apps . tuple $ length ps) . sequenceA
                           <$> traverse inferPatternType ps
inferPatternType (LitP l)    = pure . pure $ literalType l
inferPatternType (ConP _ _ ) = undefined
