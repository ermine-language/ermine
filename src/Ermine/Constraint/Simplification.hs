{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ermine.Constraint.Simplification
  ( bySuper
  , bySupers
  , byInstance
  , entails
  , simplifyVia
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad hiding (sequence)
import Control.Monad.Trans.Maybe
import Data.List (delete)
import Data.Foldable hiding (all)
import Data.Map as Map hiding (map, empty, delete, filter)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable
import Data.Void
import Ermine.Constraint.Env
import Ermine.Syntax
import Ermine.Syntax.Core as Core hiding (App, Var)
import Ermine.Syntax.Id
import Ermine.Syntax.Head
import Ermine.Syntax.Instance as Instance
import Ermine.Syntax.Type as Type
import Prelude hiding (sequence)

mangle :: (Applicative f, Monad t, Traversable t) =>  (a -> f (t c)) -> t a -> f (t c)
mangle f c = join <$> traverse f c

-- Determines whether the first argument is above the second in the
-- instance hierarchy. Yields the Core to project out the superclass
-- dictionary.
--
-- We expect here that both arguments have been reduced by instance before
-- they arrive here.
bySuper :: (Alternative m, MonadConstraint s m, Eq k, Eq t)
                  => Type k t -> Type k t -> m (Scope b Core (Type k t))
bySuper c d = Prelude.foldr id (pure d) <$> go [] d
 where
 go ps c'
   | c == c'   = pure ps
   | otherwise = superclasses c' >>= asum . zipWith (\i -> go (super i:ps)) [0..]

-- As bySuper.
bySupers :: (Alternative m, MonadConstraint s m, Eq k, Eq t)
                   => Type k t -> [Type k t] -> m (Scope b Core (Type k t))
bySupers c cs = asum (map (bySuper c) cs)
                      >>= mangle (\d ->
                              d `bySupers` delete d cs <|>
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

 matchArg (Var i)      t                       = Just [(i, [t])]
 matchArg (App f x)    (App g y)               = (++) <$> matchArg f g <*> matchArg x y
 matchArg (HardType h) (HardType h') | h == h' = Just []
 matchArg _            _                       = Nothing

byInstance :: (Eq k, Eq t, Alternative m, MonadConstraint s m) => Type k t -> m (Scope b Core (Type k t))
byInstance = peel []
 where
 peel stk (App f x) = peel (x:stk) f
 peel stk (HardType (Con g _)) = viewConstraint (instances . at g) >>= \ xs -> case xs of
   Nothing -> empty
   Just [] -> empty
   Just is -> case catMaybes $ tryConstraint stk <$> is of
     []  -> empty
     [x] -> pure x
     _   -> fail "Ambiguous class discharge"
 peel _   _ = error "PANIC: Malformed class head"

 tryConstraint stk i = matchHead stk (i^.instanceHead) <&> \m ->
   Prelude.foldl (\ r a -> _AppDict # (r, pure $ join $ bimap absurd (m!) a)) (_Id._InstanceId # (i^.instanceHead)) $ i^.instanceContext

entails :: (Alternative m, MonadConstraint s m, Eq k, Eq t) => [Type k t] -> Type k t -> m (Scope b Core (Type k t))
entails cs c =
  c `bySupers` cs' <|> (byInstance c >>= simplifyVia cs)
 where cs' = filter (/= c) cs

simplifyVia :: (MonadConstraint s m, Eq k, Eq t) => [Type k t] -> Scope b Core (Type k t) -> m (Scope b Core (Type k t))
simplifyVia cs s = do
  x <- for s $ \t -> runMaybeT (entails cs t) <&> fromMaybe (pure t)
  return $ join x
