{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ermine.Constraint.Env
  ( MonadConstraint(..)
  , D(..)
  , ConstraintEnv(..)
  , HasConstraintEnv(..)
  , dummyConstraintEnv
  , viewConstraint
  , superclasses
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad hiding (sequence)
import Control.Monad.ST.Class
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.Map as Map hiding (map, empty, delete, filter)
import Data.Text (unpack)
import Data.Void
import Ermine.Builtin.Type as Type
import Ermine.Syntax.Class
import Ermine.Syntax.Core as Core hiding (App, Var)
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Head
import Ermine.Syntax.Hint
import Ermine.Syntax.Instance as Instance
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Literal
import Ermine.Syntax.Name
import Ermine.Syntax.Type as Type
import Ermine.Unification.Meta
import Prelude hiding (sequence)

data ConstraintEnv = ConstraintEnv
  { _classes   :: Map Global Class
  , _instances :: Map Global [Instance]
  }

makeClassy ''ConstraintEnv

dummyConstraintEnv :: ConstraintEnv
dummyConstraintEnv = ConstraintEnv { _classes = singleton lame clame
                                 , _instances = singleton lame [ilame]
                                 }
 where
 -- class Lame a where lame :: a
 -- instance Lame Int where lame = 5
 clame = Class [] [Unhinted $ Scope star] []
 hlame = mkHead lame 0 [] [] [Type.int]
 dlame = Dict [] [_Lit # Int 5]
 ilame = Instance [] hlame dlame

class MonadMeta s m => MonadConstraint s m where
  askConstraint :: m ConstraintEnv
  localConstraint :: (ConstraintEnv -> ConstraintEnv) -> m a -> m a

instance MonadConstraint s m => MonadConstraint s (MaybeT m) where
  askConstraint     = MaybeT $ fmap Just askConstraint
  localConstraint f = MaybeT . localConstraint f . runMaybeT


newtype D s a = D { discharging :: ConstraintEnv -> M s a }

instance Functor (D s) where
  fmap f (D e) = D $ fmap f <$> e

instance Applicative (D s) where
  pure = D . pure . pure
  D f <*> D x = D $ liftM2 (<*>) f x

instance Monad (D s) where
  return = pure
  D m >>= f = D $ \e -> m e >>= \x -> discharging (f x) e

instance MonadST (D s) where
  type World (D s) = s
  liftST m = D . const $ liftST m

instance MonadMeta s (D s) where
  askMeta = D $ const askMeta
  localMeta f (D m) = D $ localMeta f . m

instance MonadConstraint s (D s) where
  askConstraint = D $ \de -> pure de
  localConstraint f (D m) = D $ \de -> m $ f de

viewConstraint :: MonadConstraint s m => Getting a ConstraintEnv a -> m a
viewConstraint g = view g <$> askConstraint

superclasses :: MonadConstraint s m => Type k t -> m [Type k t]
superclasses tc = go [] tc
 where
 go stk (App f x) = go (x:stk) f
 go stk (HardType (Con g _)) = viewConstraint (classes.at g) >>= \case
   Just clazz -> traverse (fmap join . bitraverse (pure . absurd) look) $ clazz^.context
    where look i | Just t <- stk ^? ix i = pure t
                 | otherwise = fail "Under-applied class"
   Nothing -> fail $ "Unknown class: " ++ unpack (g^.name)
 go _   _ = fail "Malformed class"

