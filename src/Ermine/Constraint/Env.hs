{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ermine.Constraint.Env
  ( MonadConstraint(..)
  , CM(..)
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
import Ermine.Builtin.Global (fromIntegerToIntg, fromIntegerToLongg)
import Ermine.Builtin.Head
import Ermine.Builtin.Type as Type
import Ermine.Syntax.Class
import Ermine.Syntax.Core as Core hiding (App, Var)
import Ermine.Syntax.Global as Global
import Ermine.Syntax.Instance as Instance
import Ermine.Syntax.Kind as Kind hiding (Var)
import Ermine.Syntax.Literal
import Ermine.Syntax.Name
import Ermine.Syntax.Type as Type
import Ermine.Unification.Meta
import Prelude hiding (sequence)

data ConstraintEnv cc = ConstraintEnv
  { _classes   :: Map Global Class
  , _instances :: Map Global [Instance cc]
  }

makeClassy ''ConstraintEnv

dummyConstraintEnv :: ConstraintEnv cc
dummyConstraintEnv = ConstraintEnv
  { _classes = fromList [(lame, clame), (fromInteg, cfromInteger)]
  , _instances = fromList [ (lame, [ilameI, ilameL])
                          , (fromInteg, [ifromIntegerI, ifromIntegerL])
                          ]
  }
 where
 -- class Lame a where lame :: a
 clame = Class [] [(Just "a", Scope star)] []
 -- instance Lame Int where lame = 5
 dlameI = Dict [] [_Lit # Int 5]
 ilameI = Instance [] hlameI dlameI
 -- instance Lame Long where lame = 37
 dlameL = Dict [] [_Lit # Long 37]
 ilameL = Instance [] hlameL dlameL

 -- class FromInteger a where fromInteger :: Integer -> a
 cfromInteger = Class [] [(Just "a", Scope star)] []
 -- instance FromInteger Int where fromInteger = fromIntegerToInt
 dfromIntegerI = Dict [] [_Global # fromIntegerToIntg]
 ifromIntegerI = Instance [] hfromIntegerI dfromIntegerI

 -- instance FromInteger Long where fromInteger = fromIntegerToLong
 dfromIntegerL = Dict [] [_Global # fromIntegerToLongg]
 ifromIntegerL = Instance [] hfromIntegerL dfromIntegerL

class (cc ~ Cv m, MonadMeta s m) => MonadConstraint cc s m where
  type Cv m
  askConstraint :: m (ConstraintEnv cc)
  localConstraint
    :: (ConstraintEnv cc -> ConstraintEnv cc) -> m a -> m a

instance MonadConstraint cc s m => MonadConstraint cc s (MaybeT m) where
  type Cv (MaybeT m) = Cv m
  askConstraint     = MaybeT $ fmap Just askConstraint
  localConstraint f = MaybeT . localConstraint f . runMaybeT

newtype CM cc s a = CM { runCM :: ConstraintEnv cc -> M s a }

instance Functor (CM cc s) where
  fmap f (CM e) = CM $ fmap f <$> e

instance Applicative (CM cc s) where
  pure = CM . pure . pure
  CM f <*> CM x = CM $ liftM2 (<*>) f x

instance Monad (CM cc s) where
  return = pure
  CM m >>= f = CM $ \e -> m e >>= \x -> runCM (f x) e

instance MonadST (CM cc s) where
  type World (CM cc s) = s
  liftST m = CM . const $ liftST m

instance MonadMeta s (CM cc s) where
  askMeta = CM $ const askMeta
  localMeta f (CM m) = CM $ localMeta f . m

instance MonadConstraint cc s (CM cc s) where
  type Cv (CM cc s) = cc
  askConstraint = CM $ \de -> pure de
  localConstraint f (CM m) = CM $ \de -> m $ f de

viewConstraint :: MonadConstraint cc s m => Getting a (ConstraintEnv cc) a -> m a
viewConstraint g = view g <$> askConstraint

superclasses :: MonadConstraint cc s m => Type k t -> m [Type k t]
superclasses = go []
 where
 go stk (App f x) = go (x:stk) f
 go stk (HardType (Con g _)) = viewConstraint (classes.at g) >>= \ xs -> case xs of
   Just clazz -> traverse (fmap join . bitraverse (pure . absurd) look) $ clazz^.context
    where look i | Just t <- stk ^? ix i = pure t
                 | otherwise = fail "Under-applied class"
   Nothing -> fail $ "Unknown class: " ++ unpack (g^.name)
 go _   _ = fail "Malformed class"

