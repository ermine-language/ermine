{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Kind.Inference
-- Copyright :  (c) Edward Kmett 2011-2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Kind.Inference
  ( inferKind
  , checkKind
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.Void
import Ermine.Diagnostic
import Ermine.Kind as Kind
import Ermine.Kind.Unification
import Ermine.Meta
import Ermine.Scope
import qualified Ermine.Type as Type
import Ermine.Type hiding (Var)

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: KindM s -> M s (KindM s, KindM s)
matchFunKind (a :-> b)    = return (a, b)
matchFunKind (HardKind _) = fail "not a fun kind"
matchFunKind (Var kv) = do
  a <- Var <$> newMeta ()
  b <- Var <$> newMeta ()
  (a, b) <$ runWriterT (unifyKindVar mempty kv (a :-> b))

instantiateSchema :: Schema (MetaK s) -> M s (KindM s)
instantiateSchema (Schema n s) = do
  vs <- replicateM n (Var <$> newMeta ())
  return $ instantiate (vs!!) s
{-# INLINE instantiateSchema #-}

-- | Check that the 'Kind' of a given 'Type' can unify with the specified kind.
checkKind :: Type (MetaK s) (KindM s) -> KindM s -> M s ()
checkKind t k = do
  k' <- inferKind t
  () <$ runWriterT (unifyKind mempty k k')

-- | Infer a kind for a given type.
inferKind :: Type (MetaK s) (KindM s) -> M s (KindM s)
inferKind (Loc l t)                = local (set rendering l) $ inferKind t
inferKind (Type.Var tk)            = return tk
inferKind (HardType Arrow)         = return $ star :-> star :-> star
inferKind (HardType (Con _ s))     = instantiateSchema (vacuous s)
inferKind (HardType (Tuple n))     = return $ productKind n
inferKind (HardType ConcreteRho{}) = return rho
inferKind (App f x) = do
  kf <- inferKind f
  (a, b) <- matchFunKind kf
  b <$ checkKind x a
inferKind (Exists ks cs) = do
  for_ cs $ \c ->
    checkKind (instantiateVars ks c) constraint
  return constraint
inferKind (Forall n tks cs b) = do
  sks <- replicateM n $ newSkolem ()
  let btys = instantiateVars sks <$> tks
  for_ cs $ \c ->
    checkKind (instantiateKindVars sks (instantiateVars btys c)) constraint
  checkKind (instantiateKindVars sks (instantiateVars btys b)) star
  return star
