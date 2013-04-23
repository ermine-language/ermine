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
module Ermine.Inference.Kind
  ( inferKind
  , checkKind
  , checkDataTypeKind
  , checkConstructorKind
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.IntSet.Lens
import Data.Traversable (for)
import Data.Void
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.DataType as Data
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Scope
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var)
import Ermine.Unification.Kind
import Ermine.Unification.Meta

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: KindM s -> M s (KindM s, KindM s)
matchFunKind (a :-> b)    = return (a, b)
matchFunKind (HardKind _) = fail "not a fun kind"
matchFunKind (Var kv) = do
  a <- Var <$> newMeta ()
  b <- Var <$> newMeta ()
  (a, b) <$ runWriterT (unifyKindVar kv (a :-> b))

instantiateSchema :: Schema (MetaK s) -> M s (KindM s)
instantiateSchema (Schema hs s) = do
  vs <- forM hs (\_ -> Var <$> newMeta ())
  return $ instantiate (vs!!) s
{-# INLINE instantiateSchema #-}

-- | Check that the 'Kind' of a given 'Type' can unify with the specified kind.
checkKind :: Type (MetaK s) (KindM s) -> KindM s -> M s ()
checkKind t k = do
  k' <- inferKind t
  () <$ runWriterT (unifyKind k k')

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
inferKind (And cs) = constraint <$ traverse_ (checkKind ?? constraint) cs
inferKind (Exists n tks cs) = do
  sks <- for n $ \_ -> newSkolem ()
  let btys = instantiateVars sks . extract <$> tks
  checkKind (instantiateKindVars sks $ instantiateVars btys cs) constraint
  return constraint
inferKind (Forall n tks cs b) = do
  sks <- for n $ \_ -> newSkolem ()
  let btys = instantiateVars sks . extract <$> tks
  checkKind (instantiateKindVars sks (instantiateVars btys cs)) constraint
  checkKind (instantiateKindVars sks (instantiateVars btys b)) star
  return star

-- | Checks that the types in a data declaration have sensible kinds.
checkDataTypeKind :: KindM s -> DataType (MetaK s) (KindM s) -> M s (Schema a)
checkDataTypeKind self (DataType nm ks ts cs) = do
  sks <- for ks $ \_ -> newSkolem ()
  let btys = instantiateVars sks . extract <$> ts
  for_ cs $ \c -> checkConstructorKind
                    (bimap (unvar (sks!!) id) (unvar (btys !!) id) c)
  checkKind (apps (pure self) $ pure <$> btys) star
  generalizeOver (setOf (traverse.metaId) sks) self

-- | Checks that the types in a data constructor have sensible kinds.
checkConstructorKind :: Constructor (MetaK s) (KindM s) -> M s ()
checkConstructorKind (Constructor _ ks ts fields) = do
  sks <- for ks $ \_ -> newSkolem ()
  let btys = instantiateVars sks . extract <$> ts
  for_ fields $ \fld -> checkKind (instantiateKindVars sks $ instantiateVars btys fld) star
