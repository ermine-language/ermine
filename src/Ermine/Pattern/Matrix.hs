{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel and Edward Kmett 2013-2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pattern.Matrix
  (
  -- * Pattern Matrix
    PatternMatrix(..)
  , HasPatternMatrix(..)
  , defaultOn
  , splitOn
  -- * Claused
  , Claused(..)
  , hoistClaused
  ) where

import Prelude hiding (all)

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List (transpose)
import Data.Word
import Ermine.Syntax.Pattern

#ifdef HLINT
{-# ANN module "hlint: ignore Eta reduce" #-}
#endif

-- * Claused

data Claused c a = Localized [Scope PatternPath (Scope Word32 c) a]
                             (Guarded (Scope PatternPath (Scope Word32 c) a))
                 | Raw (Guarded (Scope PatternPath c a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

hoistClaused :: Functor c => (forall x. c x -> d x) -> Claused c a -> Claused d a
hoistClaused tr (Raw g) = Raw $ hoistScope tr <$> g
hoistClaused tr (Localized ds g) =
  Localized (hoistScope tr' <$> ds) (hoistScope tr' <$> g)
 where
 tr' = hoistScope tr

-- * Pattern Matrix

-- | Pattern matrices for compilation. The matrix is represented as a list
-- of columns. There is also an extra column representing the guards.
data PatternMatrix t c a = PatternMatrix
  { _cols     :: [[Pattern t]]
  , _bodies   :: [Claused c a]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

makeClassy ''PatternMatrix

-- | A helper function to make a more deeply nested core.
promote :: (Functor f, Functor g, Applicative c) => f (g a) -> f (g (Var b (c a)))
promote = fmap . fmap $ F . pure

-- | Computes the matrix that should be used recursively when defaulting on
-- the specified column.
defaultOn :: Applicative c => Int -> PatternMatrix t c a -> PatternMatrix t c (Var () (c a))
defaultOn i (PatternMatrix ps cs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PatternMatrix (map select $ ls ++ rs) (promote $ select cs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

-- | Computes the matrix that should be used recursively when defaulting on
-- the specified column, with the given pattern head.
splitOn :: Applicative c
        => Int -> PatternHead -> PatternMatrix t c a -> PatternMatrix t c (Var Word8 (c a))
splitOn i hd (PatternMatrix ps cs)
  | (ls, c:rs) <- splitAt i ps = let
      con pat = traverseHead hd pat
      prune (AsP r) = prune r
      prune r = r
      p (pat, _) = has con (prune pat) || matchesTrivially pat
      select c' = map snd . filter p $ zip c c'
      newcs = transpose $ c >>= \pat -> case () of
        _ | Just ps' <- preview con pat -> [ps']
          | matchesTrivially pat        -> [replicate (fromIntegral $ hd^.arity) WildcardP]
          | otherwise                   -> []
    in PatternMatrix (map select ls ++ newcs ++ map select rs)
               (promote $ select cs)
  | otherwise = error "PANIC: splitOn: bad column reference"
