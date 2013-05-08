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
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IntMap as IntMap
import Data.List as List (partition)
import Data.Traversable
import Ermine.Builtin.Type as Builtin
import Ermine.Syntax
import Ermine.Syntax.Id
import Ermine.Syntax.Literal
import Ermine.Syntax.Core as Core
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Scope
import Ermine.Syntax.Term as Term
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var, Loc)
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Trifecta.Result

type WitnessM s = Witness (MetaK s) (MetaT s)

type TermM s = Term (Annot (MetaK s) (MetaT s)) (TypeM s)

matchFunType :: MonadMeta s m => TypeM s -> m (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ unsharingT (unifyType t (x ~> y))

-- discharge :: [TypeM s] -> TypeM s -> M s (Maybe ([TypeM s], Core (Var (Either Int Int) Id)

inferType :: MonadMeta s m => TermM s -> m (WitnessM s)
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
  Witness fcs frcs ft fc <- inferType f
  (i, o) <- matchFunType ft
  Witness xcs xrcs _ xc <- checkType x i
  simplifiedWitness (IntMap.union fcs xcs) (frcs ++ xrcs) o $ Core.App fc xc
inferType _ = fail "Unimplemented"

-- TODO: write this
simplifiedWitness :: MonadMeta s m => IntMap (TypeM s) -> [TypeM s] -> TypeM s -> Core (Var Int Id) -> m (WitnessM s)
simplifiedWitness cs rcs t c = return $ Witness cs rcs t c

checkType :: MonadMeta s m => TermM s -> TypeM s -> m (WitnessM s)
checkType _ _ = fail "Check yourself"

inferHardType :: MonadMeta s m => HardTerm -> m (WitnessM s)
inferHardType (Term.Lit l) = return $ Witness mempty [] (literalType l) (HardCore (Core.Lit l))
inferHardType (Term.Tuple n) = do
  vars <- replicateM n $ pure <$> newMeta star
  return $ Witness mempty [] (Prelude.foldr (~>) (tup vars) vars) $ dataCon n 0
inferHardType Hole = do
  tv <- newMeta star
  r <- viewMeta metaRendering
  return $ Witness mempty [] (Type.Var tv) $ HardCore $ Core.Error $ show $ plain $ explain r $ Err (Just (text "open hole")) [] mempty
inferHardType _ = fail "Unimplemented"

literalType :: Literal -> Type k a
literalType Int{}    = Builtin.int
literalType Long{}   = long
literalType Byte{}   = byte
literalType Short{}  = short
literalType String{} = Builtin.string
literalType Char{}   = Builtin.char
literalType Float{}  = Builtin.float
literalType Double{} = Builtin.double

unfurl :: MonadMeta s m => TypeM s -> Core (Var Int Id) -> m (WitnessM s)
unfurl (Forall ks ts cs bd) co = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  let inst = instantiateKindVars mks . instantiateVars mts
  (rcs, tcs) <- unfurlConstraints . inst $ cs
  rcm <- for rcs $ \x -> do i <- fresh; return (i, x)
  return $ Witness (IntMap.fromList rcm) tcs (inst bd) co
unfurl t co = pure $ Witness mempty [] t co

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
