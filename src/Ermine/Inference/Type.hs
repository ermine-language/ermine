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
  , MonadDischarge(..)
  , dischargesBySuper
  , dischargesBySupers
  , dischargesByInstance
  , entails
  , simplifyVia
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
import Data.List as List (partition, delete)
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
import Ermine.Inference.Discharge
import Ermine.Inference.Witness
import Ermine.Unification.Type
import Ermine.Unification.Meta
import Ermine.Unification.Sharing
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)
import Text.Trifecta.Result

type WitnessM s = Witness (MetaK s) (MetaT s)

type TermM s = Term (Annot (MetaK s) (MetaT s)) (TypeM s)

type CoreM s = Core (Var Id (TypeM s))

matchFunType :: MonadMeta s m => TypeM s -> m (TypeM s, TypeM s)
matchFunType (Type.App (Type.App (HardType Arrow) a) b) = return (a, b)
matchFunType (HardType _) = fail "not a fun type"
matchFunType t = do
  x <- pure <$> newMeta star
  y <- pure <$> newMeta star
  (x, y) <$ unsharingT (unifyType t (x ~> y))

-- discharge :: [TypeM s] -> TypeM s -> M s (Maybe ([TypeM s], Core (Var (Either Int Int) Id)

coreBind :: (a -> Core (Var b c)) -> Core (Var b a) -> Core (Var b c)
coreBind f c = c >>= unvar (pure . B) f

coreMangle :: Applicative f =>  (a -> f (Core (Var b c))) -> Core (Var b a) -> f (Core (Var b c))
coreMangle f c = coreBind id <$> traverse (traverse f) c

-- Determines whether the first argument is above the second in the
-- instance hierarchy. Yields the Core to project out the superclass
-- dictionary.
--
-- We expect here that both arguments have been reduced by instance before
-- they arrive here.
--
-- c `dischargesBySuper` c'
dischargesBySuper :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
                  => Type k t -> Type k t -> m (Core (Var b (Type k t)))
dischargesBySuper c d = Prelude.foldr ($) (pure $ F d) <$> go [] d
 where
 go ps c'
   | c == c'   = pure $ ps
   | otherwise = superclasses c' >>= asum . zipWith (\i -> go (super i:ps)) [1..]

-- As dischargesBySuper.
dischargesBySupers :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
                   => Type k t -> [Type k t] -> m (Core (Var b (Type k t)))
dischargesBySupers c cs = asum (map (dischargesBySuper c) cs)
                      >>= coreMangle (\d ->
                              d `dischargesBySupers` delete d cs <|>
                              pure (pure $ pure d))

dischargesByInstance :: (Alternative m, MonadDischarge s m)
                     => Type k t -> m (Core (Var Id (Type k t)))
dischargesByInstance _ = empty

entails :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
        => [Type k t] -> Type k t -> m (Core (Var Id (Type k t)))
entails cs c = c `dischargesBySupers` cs <|> (dischargesByInstance c >>= simplifyVia cs)

simplifyVia :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
            => [Type k t] -> Core (Var Id (Type k t)) -> m (Core (Var Id (Type k t)))
simplifyVia cs = coreMangle (\c -> entails cs c <|> pure (pure $ F c))

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
  Witness frcs ft fc <- inferType f
  (i, o) <- matchFunType ft
  Witness xrcs _ xc <- checkType x i
  simplifiedWitness (frcs ++ xrcs) o $ Core.App fc xc
inferType _ = fail "Unimplemented"

-- TODO: write this
simplifiedWitness :: MonadMeta s m => [TypeM s] -> TypeM s -> Core (Var Id (TypeM s)) -> m (WitnessM s)
simplifiedWitness rcs t c = return $ Witness rcs t c

checkType :: MonadMeta s m => TermM s -> TypeM s -> m (WitnessM s)
checkType _ _ = fail "Check yourself"

inferHardType :: MonadMeta s m => HardTerm -> m (WitnessM s)
inferHardType (Term.Lit l) = return $ Witness [] (literalType l) (HardCore (Core.Lit l))
inferHardType (Term.Tuple n) = do
  vars <- replicateM n $ pure <$> newMeta star
  return $ Witness [] (Prelude.foldr (~>) (tup vars) vars) $ dataCon n 0
inferHardType Hole = do
  tv <- newMeta star
  r <- viewMeta metaRendering
  return $ Witness [] (Type.Var tv) $ HardCore $ Core.Error $ show $ plain $ explain r $ Err (Just (text "open hole")) [] mempty
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

unfurl :: MonadMeta s m => TypeM s -> Core Id -> m (WitnessM s)
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
