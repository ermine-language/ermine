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
  , generalize
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.Void
import Data.IntMap as IntMap
import Ermine.Kind as Kind
import Ermine.Kind.Unification
import Ermine.Meta
import qualified Ermine.Type as Type
import           Ermine.Type hiding (Var)

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

checkKind :: Type (MetaK s) (KindM s) -> KindM s -> M s ()
checkKind t k = do
  k' <- inferKind t
  () <$ runWriterT (unifyKind mempty k k')

instantiateList :: Monad t => [a] -> Scope Int t a -> t a
instantiateList as = instantiate (return . (as !!))
{-# INLINE instantiateList #-}

inferKind :: Type (MetaK s) (KindM s) -> M s (KindM s)
inferKind (Loc l t)                = local (const l) $ inferKind t
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
    checkKind (instantiateList ks c) constraint
  return constraint
inferKind (Forall n tks cs b) = do
  sks <- replicateM n $ newSkolem ()
  let btys = instantiateList sks <$> tks
  for_ cs $ \c ->
    checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys c)) constraint
  checkKind (instantiateKinds (pure . (sks!!)) (instantiateList btys b)) star
  return star

-- | Generalize a kind, checking for escaped Skolems.
generalize :: KindM s -> M s (Schema a)
generalize k0 = do
  k <- zonk mempty k0
  (r,(_,n)) <- runStateT (traverse go k) (IntMap.empty, 0)
  return $ Schema n (Scope r)
  where
   go Skolem{}   = StateT $ \ _ -> fail "escaped skolem"
   go (Meta _ i _ _ _) = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))
