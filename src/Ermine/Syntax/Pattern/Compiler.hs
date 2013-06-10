{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Syntax.Pattern.Compiler
  ( Guard(..)
  , PMatrix(..)
  , defaultOn
  , splitConOn
  ) where

import Data.Foldable
import Data.List (transpose)
import Data.Traversable
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern
import Ermine.Syntax.Term

-- | Guard representation. Every row of a pattern matrix must have an
-- assocated guard, but if no guard was specified, then it will simply be
-- the trivial guard which allows everything.
data Guard t v = Trivial | Explicit (Term t v)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Pattern matrices for compilation. The matrix is represented as a list
-- of columns. There is also an extra column representing the guards.
data PMatrix t v = PMatrix [[Pattern t]] [Guard t v]
  deriving (Eq, Show)

defaultOn :: Int -> PMatrix t v -> PMatrix t v
defaultOn i (PMatrix ps gs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PMatrix (map select $ ls ++ rs) (select gs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

splitConOn :: Int -> Global -> PMatrix t v -> PMatrix t v
splitConOn i g (PMatrix ps gs)
  | (ls, c:rs) <- splitAt i ps = let
      p (ConP g' _, _) = g == g'
      p _              = False
      select c' = map snd . filter p $ zip c c'
      newcs = transpose [ ps' | ConP g' ps' <- c, g == g' ]
    in PMatrix (map select ls ++ newcs ++ map select rs) (select gs)
  | otherwise = error "PANIC: splitConOn: bad column reference"
