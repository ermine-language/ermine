{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ermine.Inference.Discharge
  ( MonadDischarge(..)
  , D(..)
  , DischargeEnv(..)
  , HasDischargeEnv(..)
  , dummyDischargeEnv
  , viewDischarge
  , superclasses
  , dischargesBySuper
  , dischargesBySupers
  , dischargesByInstance
  , entails
  , simplifyVia
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad hiding (sequence)
import Control.Monad.ST.Class
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.List (delete)
import Data.Foldable hiding (all)
import Data.Map as Map hiding (map, empty, delete, filter)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (unpack)
import Data.Traversable
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
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Name
import Ermine.Syntax.Type as Type
import Ermine.Unification.Meta
import Prelude hiding (sequence)

data DischargeEnv = DischargeEnv
                  { _classes   :: Map Global Class
                  , _instances :: Map Global [Instance]
                  }

makeClassy ''DischargeEnv

dummyDischargeEnv :: DischargeEnv
dummyDischargeEnv = DischargeEnv { _classes = singleton lame clame
                                 , _instances = singleton lame [ilame]
                                 }
 where
 -- class Lame a where lame :: a
 -- instance Lame Int where lame = 5
 lame = glob Idfix (mkModuleName_ "Ermine") "Lame"
 clame = Class [] [Unhinted $ Scope star] []
 hlame = mkHead lame 0 [] [] [Type.int]
 dlame = Dict [] [_Lit # Int 5]
 ilame = Instance [] hlame dlame

class MonadMeta s m => MonadDischarge s m where
  askDischarge :: m DischargeEnv
  localDischarge :: (DischargeEnv -> DischargeEnv) -> m a -> m a

instance MonadDischarge s m => MonadDischarge s (MaybeT m) where
  askDischarge     = MaybeT $ fmap Just askDischarge
  localDischarge f = MaybeT . localDischarge f . runMaybeT


newtype D s a = D { discharging :: DischargeEnv -> M s a }

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

instance MonadDischarge s (D s) where
  askDischarge = D $ \de -> pure de
  localDischarge f (D m) = D $ \de -> m $ f de

mangle :: (Applicative f, Monad t, Traversable t) =>  (a -> f (t c)) -> t a -> f (t c)
mangle f c = join <$> traverse f c

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
dischargesBySuper :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
                  => Type k t -> Type k t -> m (Scope b Core (Type k t))
dischargesBySuper c d = Prelude.foldr id (pure d) <$> go [] d
 where
 go ps c'
   | c == c'   = pure $ ps
   | otherwise = superclasses c' >>= asum . zipWith (\i -> go (super i:ps)) [0..]

-- As dischargesBySuper.
dischargesBySupers :: (Alternative m, MonadDischarge s m, Eq k, Eq t)
                   => Type k t -> [Type k t] -> m (Scope b Core (Type k t))
dischargesBySupers c cs = asum (map (dischargesBySuper c) cs)
                      >>= mangle (\d ->
                              d `dischargesBySupers` delete d cs <|>
                              pure (pure d))

-- Checks if a series of type arguments would match against the type
-- type \"patterns\" of an instance head. Returns a mapping from the
-- variables bound in the instance to the types they would correspond to
-- after the match.
--
-- Basic equality is used, as no unification should be caused by this
-- action. This means that the types should be fully zonked when passed in,
-- or the results will be erroneous.
matchHead :: (Eq k, Eq t) => [Type k t] -> Head -> Maybe (Map Int (Type k t))
matchHead ts he = join . fmap (traverse check . fromListWith (++) . join)
                . sequence $ zipWith matchArg (he^.headTypeArgs) ts
 where
 check [x]        = Just x
 check (x:xs)
   | all (x==) xs = Just x
 check _          = Nothing

 matchArg (Var i)      t             = Just [(i, [t])]
 matchArg _            (Var _)       = Nothing
 matchArg (App f x)    (App g y)     = (++) <$> matchArg f g <*> matchArg x y
 matchArg (HardType h) (HardType h') = if h == h' then Just [] else Nothing
 matchArg _            _             = error "PANIC: matchHead encountered invalid Head or constraint."

dischargesByInstance :: (Eq k, Eq t, Alternative m, MonadDischarge s m) => Type k t -> m (Scope b Core (Type k t))
dischargesByInstance c = peel [] c
 where
 peel stk (App f x) = peel (x:stk) f
 peel stk (HardType (Con g _)) = viewDischarge (instances . at g) >>= \case
   Nothing -> empty
   Just [] -> empty
   Just is -> case catMaybes $ tryDischarge stk <$> is of
     []  -> empty
     [x] -> pure x
     _   -> fail "Ambiguous class discharge"
 peel _   _ = error "PANIC: Malformed class head"

 tryDischarge stk i = matchHead stk (i^.instanceHead) <&> \m ->
   Prelude.foldl (\ r a -> _AppDict # (r, pure $ join $ bimap absurd (m!) a)) (_InstanceId # (i^.instanceHead)) $ i^.instanceContext

entails :: (Alternative m, MonadDischarge s m, Eq k, Eq t) => [Type k t] -> Type k t -> m (Scope b Core (Type k t))
entails cs c =
  c `dischargesBySupers` cs' <|> (dischargesByInstance c >>= simplifyVia cs)
 where cs' = filter (/= c) cs

simplifyVia :: (MonadDischarge s m, Eq k, Eq t) => [Type k t] -> Scope b Core (Type k t) -> m (Scope b Core (Type k t))
simplifyVia cs s = do
  x <- for s $ \t -> runMaybeT (entails cs t) <&> fromMaybe (pure t)
  return $ join x
