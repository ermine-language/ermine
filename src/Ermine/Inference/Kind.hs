{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , inferAnnotKind
  , checkKind
  , checkDataTypeKinds
  , checkDataTypeGroup
  , inferDataTypeKind
  , checkConstructorKind
  ) where

import Bound
import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable hiding (concat, foldr)
import Data.Graph (stronglyConnComp, flattenSCC)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Void
import Ermine.Diagnostic
import Ermine.Syntax
import Ermine.Syntax.Constructor as Data
import Ermine.Syntax.Data as Data
import Ermine.Syntax.Global
import Ermine.Syntax.Kind as Kind
import Ermine.Syntax.Name
import Ermine.Syntax.Scope
import qualified Ermine.Syntax.Type as Type
import Ermine.Syntax.Type hiding (Var)
import Ermine.Unification.Data
import Ermine.Unification.Kind
import Ermine.Unification.Meta
import Ermine.Unification.Sharing

productKind :: Int -> Kind k
productKind 0 = star
productKind n = star :-> productKind (n - 1)

matchFunKind :: MonadMeta s m => KindM s -> m (KindM s, KindM s)
matchFunKind (a :-> b)    = return (a, b)
matchFunKind (Var kv) = do
  a <- Var <$> newMeta False Nothing
  b <- Var <$> newMeta False Nothing
  (a, b) <$ unsharingT (unifyKindVar kv (a :-> b))
matchFunKind (Type _)     = fail "not a fun kind"
matchFunKind (HardKind _) = fail "not a fun kind"

instantiateSchema :: MonadMeta s m => Schema (MetaK s) -> m (KindM s)
instantiateSchema (Schema hs s) = do
  vs <- forM hs (fmap Var . newMeta False)
  return $ instantiate (vs!!) s
{-# INLINE instantiateSchema #-}

-- | Check that the 'Kind' of a given 'Type' can unify with the specified kind.
checkKind :: MonadMeta s m => Type (MetaK s) (KindM s) -> KindM s -> m ()
checkKind t k = do
  k' <- inferKind t
  () <$ unsharingT (unifyKind k k')

-- | Infer a kind for a given type.
inferKind :: MonadMeta s m => Type (MetaK s) (KindM s) -> m (KindM s)
inferKind (Loc l t)                = set rendering l `localMeta` inferKind t
inferKind (Type.Var tk)            = return tk
inferKind (HardType Arrow)         = do
  a <- Var <$> newMeta True Nothing
  b <- Var <$> newMeta True Nothing
  return $ Type a :-> Type b :-> star
inferKind (HardType (Con _ s))     = instantiateSchema (vacuous s)
inferKind (HardType (Tuple n))     = return $ productKind n
inferKind (HardType ConcreteRho{}) = return rho
inferKind (App f x) = do
  kf <- inferKind f
  (a, b) <- matchFunKind kf
  b <$ checkKind x a
inferKind (And cs) = constraint <$ traverse_ (checkKind ?? constraint) cs
inferKind (Exists n tks cs) = do
  sks <- for n $ newMeta False
  let btys = instantiateVars sks . extract <$> tks
  checkKind (instantiateKindVars sks $ instantiateVars btys cs) constraint
  -- TODO: check mutually exclusive sks?
  return constraint
inferKind (Forall n tks cs b) = do
  sks <- for n $ newMeta False
  let btys = instantiateVars sks . extract <$> tks
  checkKind (instantiateKindVars sks (instantiateVars btys cs)) constraint
  checkKind (instantiateKindVars sks (instantiateVars btys b)) star
  -- TODO: check mutually exclusive sks?
  return star

inferAnnotKind :: MonadMeta s m => Annot (MetaK s) (KindM s) -> m (KindM s)
inferAnnotKind (Annot hs hts ty) = do
  ks <- for hs $ fmap pure . newMeta False
  ts <- for hts $ fmap pure . newMeta False
  inferKind . instantiateKinds (ks!!) . instantiateVars ts $ ty

fixCons :: (Ord t) => Map t (Type k u) -> (t -> Type k u) -> DataType k t -> DataType k u
fixCons m f = boundBy (\t -> fromMaybe (f t) $ Map.lookup t m)

-- | Checks a list of data types for well-kindedness. The unit in the kind
-- variables is interpreted as an unknown, which must be determined by the
-- algorithm. The string variables may refer to other data types in the
-- group. During the process, all unknowns will be solved or generalized,
-- and all type variables will be replaced by the respective constructor
-- with the appropriate schematic type. These are returned from the
-- procedure.
--
-- TODO: Currently, the following example fails with a skolem escape:
--
-- @
--   data Foo (a : k) = Foo (Bar a)
--   data Bar a = Bar (Foo a)
-- @
--
-- This is because Foo and Bar must be checked as a group, k is skolemized
-- for the checking of Foo, but its type leaks to Bar's unknown kind.
-- Resolving this may be complicated, as it requires a similar
-- explicit/implicit divide as checking lets, but partial annotations are
-- possible. One solution may be to infer first and then check against
-- annotations. Kind checking is Hindley-Milner, so this is viable.
checkDataTypeKinds :: [DataType () Text] -> M s [DataType Void Void]
checkDataTypeKinds dts = flip evalStateT Map.empty
                       . fmap concat . traverse (ck.flattenSCC) $ stronglyConnComp graph
 where
 graph = map (\dt -> (dt, dt^.name, toListOf typeVars dt)) dts
 conMap = Map.fromList . map (\dt -> (dt^.name, con (dt^.global) (dataTypeSchema dt)))
 ck scc = StateT $ \m -> do
   let scc' = map (fixCons m pure) scc
   cscc <- checkDataTypeGroup scc'
   return (cscc, m `Map.union` conMap cscc)

checkDataTypeGroup :: [DataType () Text] -> M s [DataType Void Void]
checkDataTypeGroup dts = do
  insts <- traverse instantiateDataType dts
  let m = Map.fromList $ map (\(sch, dc) -> (dc^.name, sch)) insts
  inferred <- for (map snd insts) $ inferDataTypeKind m
  mess <- for (insts `zip` inferred) $ \((Schema ks s, dc), k) -> do
    sks <- for ks $ newMeta False
    let declared = instantiateVars sks s
    uncaring $ unifyKind k declared
    pure (sks, dc)
  checked <- for mess $ \(sks, dc) -> do
    () <$ checkDistinct sks
    generalizeDataCheck dc
  let cm = Map.fromList
             $ map (\dt -> (dt^.name, (dt^.global, dataTypeSchema dt))) checked
  pure $ map (boundBy $ \t -> uncurry con $ cm Map.! t) checked

-- | Checks that the types in a data declaration have sensible kinds.
inferDataTypeKind :: Map Text (Schema (MetaK s))-> DataCheck s -> M s (KindM s)
inferDataTypeKind m (DataCheck _ hks cs) = do
   cs' <- for cs $ \c -> for c $ \case
            B i -> pure $ ks !! i
            F t -> case Map.lookup t m of
              Just (Schema sks s) ->
                (instantiateVars ?? s) <$> traverse (newMeta False) sks
              Nothing -> fail "unknown reference"
   foldr (~>) star ks <$ traverse_ checkConstructorKind cs'
 where ks = map snd hks

checkConstructorKind :: Constructor (MetaK s) (KindM s) -> M s ()
checkConstructorKind (Constructor _ ks ts fs) = do
  sks <- for ks $ newShallowMeta 1 False
  let btys = instantiateVars sks . extract <$> ts
  for_ fs $ \fld ->
    checkKind (instantiateKindVars sks $ instantiateVars btys fld) star
  () <$ (checkEscapes 1 =<< checkDistinct sks)
