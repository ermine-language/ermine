{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module provides extensions to the @bound@ package.
--------------------------------------------------------------------
module Ermine.Syntax.Scope
  ( hoistScope
  , bitraverseScope
  , bound
  , free
  , BoundBy(..)
  , instantiateVars
  , putVar
  , getVar
  , putScope
  , getScope
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Data.Binary
import Data.Bitraversable

-- | Generalizes 'Bound' to permit binding by another type without taking it as a parameter.
class Monad m => BoundBy tm m | tm -> m where
  boundBy :: (a -> m b) -> tm a -> tm b

instance Monad m => BoundBy (Scope b m) m where
  boundBy = flip (>>>=)
  {-# INLINE boundBy #-}

-- | Lift a natural transformation from @f@ to @g@ into one between scopes.
hoistScope :: Functor f => (forall x. f x -> g x) -> Scope b f a -> Scope b g a
hoistScope t (Scope b) = Scope $ t (fmap t <$> b)
{-# INLINE hoistScope #-}

-- | This allows you to 'bitraverse' a 'Scope'.
bitraverseScope :: (Bitraversable t, Applicative f) => (k -> f k') -> (a -> f a') -> Scope b (t k) a -> f (Scope b (t k') a')
bitraverseScope f g = fmap Scope . bitraverse f (traverse (bitraverse f g)) . unscope
{-# INLINE bitraverseScope #-}

-- | This prism acts like a reversible smart constructor for 'B'.
bound :: Prism (Var a c) (Var b c) a b
bound = prism B $ \ t -> case t of
  B b -> Right b
  F c -> Left (F c)
{-# INLINE bound #-}

-- | This prism acts like a reversible smart constructor for 'F'.
free :: Prism (Var c a) (Var c b) a b
free = prism F $ \ t -> case t of
  B c -> Left (B c)
  F b -> Right b
{-# INLINE free #-}

-- | instantiate bound variables using a list of new variables
instantiateVars :: Monad t => [a] -> Scope Int t a -> t a
instantiateVars as = instantiate (vs !!) where
  vs = map return as
{-# INLINE instantiateVars #-}

putVar :: (b -> Put) -> (f -> Put) -> Var b f -> Put
putVar pb _  (B b) = putWord8 0 *> pb b
putVar _  pf (F f) = putWord8 1 *> pf f
{-# INLINE putVar #-}

getVar :: Get b -> Get f -> Get (Var b f)
getVar gb gf = getWord8 >>= \b -> case b of
  0 -> B <$> gb
  1 -> F <$> gf
  _ -> fail $ "getVar: Unexpected constructor code: " ++ show b
{-# INLINE getVar #-}

putScope :: (b -> Put) -> (forall a. (a -> Put) -> f a -> Put) -> (v -> Put)
         -> Scope b f v -> Put
putScope pb pf pv (Scope body) = pf (putVar pb $ pf pv) body
{-# INLINE putScope #-}

getScope :: Get b -> (forall a. Get a -> Get (f a)) -> Get v
         -> Get (Scope b f v)
getScope gb gf gv = Scope <$> gf (getVar gb $ gf gv)
{-# INLINE getScope #-}
