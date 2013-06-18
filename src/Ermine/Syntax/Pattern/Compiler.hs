{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , cols
  , guards
  , bodies
  , defaultOn
  , splitConOn
  , compile
  ) where

import Prelude hiding (all)
import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.List (transpose)
import Data.Set (Set)
import Data.Set.Lens
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern

class (Applicative m, Monad m) => MonadPComp m where
  isSignature :: Set Global -> m Bool

-- | Guard representation. Every row of a pattern matrix must have an
-- assocated guard, but if no guard was specified, then it will simply be
-- the trivial guard which allows everything.
data Guard a = Trivial | Explicit (Core a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

makePrisms ''Guard

-- | Pattern matrices for compilation. The matrix is represented as a list
-- of columns. There is also an extra column representing the guards.
data PMatrix t a = PMatrix { _cols   :: [[Pattern t]]
                           , _guards :: [Guard a]
                           , _bodies :: [Core a]
                           }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''PMatrix

defaultOn :: Int -> PMatrix t a -> PMatrix t a
defaultOn i (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PMatrix (map select $ ls ++ rs) (select gs) (select cs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

splitConOn :: Int -> Global -> PMatrix t a -> PMatrix t a
splitConOn i g (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      p (ConP g' _, _) = g == g'
      p _              = False
      select c' = map snd . filter p $ zip c c'
      newcs = transpose [ ps' | ConP g' ps' <- c, g == g' ]
    in PMatrix (map select ls ++ newcs ++ map select rs) (select gs) (select cs)
  | otherwise = error "PANIC: splitConOn: bad column reference"

patternHeads :: [Pattern t] -> Set Global
patternHeads = setOf (traverse.patternHead)

-- | Uses a heuristic to select a column from a pattern matrix.
selectCol :: [[Pattern t]] -> Maybe Int
selectCol = undefined

-- | Compiles a pattern matrix together with a corresponding set of core
-- branches to a final Core value, which will be the decision tree version
-- of the pattern matrix.
compile :: MonadPComp m => PMatrix t a -> m (Core a)
compile (PMatrix _  [] _)  = pure $ HardCore $ Error "non-exhaustive pattern match."
compile pm@(PMatrix ps gs bs)
  | all (matchesTrivially . head) ps && has _Trivial (head gs) = pure $ head bs
  | Just i <- selectCol ps = let
      heads = patternHeads $ ps !! i
      dm = defaultOn i pm
      sms = map (\g -> splitConOn i g pm) $ toListOf folded heads
    in isSignature heads >>= \sig ->
       if sig then undefined else undefined
  | otherwise = undefined

