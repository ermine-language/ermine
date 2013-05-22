{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ermine.Inference.Discharge
  ( MonadDischarge(..)
  , DischargeEnv(..)
  , supers
  , instances
  , viewDischarge
  , superclasses
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bitraversable
import Data.Map as Map
import Data.Text
import Data.Void
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Instance as Instance
import Ermine.Syntax.Type as Type
import Ermine.Unification.Meta

data DischargeEnv = DischargeEnv
                  { _supers    :: Map Global [Type Void Int]
                  , _instances :: Map Global [Instance]
                  }

makeLenses ''DischargeEnv

class MonadMeta s m => MonadDischarge s m where
  askDischarge :: m DischargeEnv
  localDischarge :: (DischargeEnv -> DischargeEnv) -> m a -> m a

viewDischarge :: MonadDischarge s m => Getter DischargeEnv a -> m a
viewDischarge g = view g <$> askDischarge

superclasses :: MonadDischarge s m => Type k t -> m [Type k t]
superclasses tc = go [] tc
 where
 go stk (App f x) = go (x:stk) f
 go stk (HardType (Con g _)) = viewDischarge (supers.at g) >>= \case
   Just ts -> traverse (fmap join . bitraverse (pure.absurd) look) ts
    where look i | Just t <- stk ^? ix i = pure t
                 | otherwise = fail "Under-applied class"
   Nothing -> fail $ "Unknown class: " ++ unpack (g^.name)
 go _   _ = fail "Malformed class"
