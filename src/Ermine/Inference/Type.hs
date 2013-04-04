{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
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
import Data.List (partition)
import Data.Traversable
import Ermine.Builtin.Type
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

type WitnessM s = Witness (MetaK s) (MetaT s)

type TermM s = Term (Annot (MetaK s) (MetaT s)) (TypeM s)

matchFunType :: TypeM s -> M s (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ runWriterT (unifyType t (x ~> y))

inferType :: TermM s -> M s (WitnessM s)
inferType (HardTerm t) = inferHardType t
inferType (Loc r tm)   = local (metaRendering .~ r) $ inferType tm
inferType (Remember i t) = do
  r <- inferType t
  remember i (r^.witnessType)
  return r
inferType (Sig tm (Annot ks ty)) = do
  ts <- for ks newMeta
  checkType tm (instantiateVars ts ty)
inferType _ = fail "Unimplemented"

checkType :: TermM s -> TypeM s -> M s (WitnessM s)
checkType _ _ = fail "Check yourself"

inferHardType :: HardTerm -> M s (WitnessM s)
inferHardType (Term.Lit l) = return $ Witness [] [] (literalType l) (HardCore (Core.Lit l))
inferHardType (Term.Tuple n) = do
  vars <- replicateM n $ pure <$> newMeta star
  return $ Witness [] [] (foldr (~>) (tup vars) vars) $
             Core.Lam n (Scope $ Core.Data 0 $ pure . B <$> [0..n-1])
inferHardType _ = fail "Unimplemented"

literalType :: Literal -> Type k a
literalType Int{}    = int
literalType Long{}   = long
literalType Byte{}   = byte
literalType Short{}  = short
literalType String{} = string
literalType Char{}   = char
literalType Float{}  = float
literalType Double{} = double

unfurl :: TypeM s -> Core (Var Int Id) -> M s (WitnessM s)
unfurl (Forall ks ts cs bd) co = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  let inst = instantiateKindVars mks . instantiateVars mts
  (rcs, tcs) <- unfurlConstraints . inst $ cs
  return $ Witness rcs tcs (inst bd) co
unfurl t co = pure $ Witness [] [] t co

partConstraints :: TypeM s -> ([TypeM s], [TypeM s])
partConstraints (And l) = partition isRowConstraint l
partConstraints c | isRowConstraint c = ([c], [])
                  | otherwise         = ([], [c])

unfurlConstraints :: TypeM s -> M s ([TypeM s], [TypeM s])
unfurlConstraints (Exists ks ts cs) = do
  mks <- for ks $ newMeta . extract
  mts <- for ts $ newMeta . instantiateVars mks . extract
  pure . partConstraints . instantiateKindVars mks . instantiateVars mts $ cs
unfurlConstraints c = pure $ partConstraints c
