{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ermine.Inference.Discharge
  ( MonadDischarge(..)
  , DischargeEnv(..)
  , classes
  , instances
  , viewDischarge
  , superclasses
  , dischargesBySuper
  , dischargesBySupers
  , dischargesByInstance
  , entails
  , simplifyVia
  ) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bitraversable
import Data.List (delete)
import Data.Foldable
import Data.Map as Map hiding (map, empty, delete)
import Data.Text (unpack)
import Data.Void
import Ermine.Syntax.Class
import Ermine.Syntax.Core as Core hiding (App)
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Id
import Ermine.Syntax.Instance as Instance
import Ermine.Syntax.Type as Type
import Ermine.Unification.Meta

data DischargeEnv = DischargeEnv
                  { _classes   :: Map Global Class
                  , _instances :: Map Global [Instance]
                  }

makeLenses ''DischargeEnv

class MonadMeta s m => MonadDischarge s m where
  askDischarge :: m DischargeEnv
  localDischarge :: (DischargeEnv -> DischargeEnv) -> m a -> m a

coreBind :: (a -> Core (Var b c)) -> Core (Var b a) -> Core (Var b c)
coreBind f c = c >>= unvar (pure . B) f

coreMangle :: Applicative f =>  (a -> f (Core (Var b c))) -> Core (Var b a) -> f (Core (Var b c))
coreMangle f c = coreBind id <$> traverse (traverse f) c

viewDischarge :: MonadDischarge s m => Getter DischargeEnv a -> m a
viewDischarge g = view g <$> askDischarge

superclasses :: MonadDischarge s m => Type k t -> m [Type k t]
superclasses tc = go [] tc
 where
 go stk (App f x) = go (x:stk) f
 go stk (HardType (Con g _)) = viewDischarge (classes.at g) >>= \case
   Just clazz -> traverse (fmap join . bitraverse (pure . absurd) look) $ clazz^.context
    where look i | Just t <- stk ^? ix i = pure t
                 | otherwise = fail "Under-applied class"
   Nothing -> fail $ "Unknown class: " ++ unpack (g^.name)
 go _   _ = fail "Malformed class"

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

