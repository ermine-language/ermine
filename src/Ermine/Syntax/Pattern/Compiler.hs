{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
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
  , MonadPComp(..)
  , CompileInfo(..)
  , pathMap
  , colCores
  , colPaths
  , cols
  , guards
  , bodies
  , defaultOn
  , splitConOn
  , compile
  ) where

import Prelude hiding (all)

import Bound
import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.List (transpose)
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import Data.Set.Lens
import Data.Traversable
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern

class (Applicative m, Monad m) => MonadPComp m where
  isSignature      :: Set Global -> m Bool
  constructorTag   :: Global -> m Int
  constructorArity :: Global -> m Int

-- | Guard representation. Every row of a pattern matrix must have an
-- assocated guard, but if no guard was specified, then it will simply be
-- the trivial guard which allows everything.
data Guard a = Trivial | Explicit (Scope PatPath Core a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

makePrisms ''Guard

data CompileInfo a = CInfo { _pathMap  :: HashMap PatPath (Core a)
                           , _colCores :: [Core a]
                           , _colPaths :: [PatPaths]
                           }

makeLenses ''CompileInfo

remove :: Int -> CompileInfo a -> CompileInfo (Var () (Core a))
remove i (CInfo m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, _:cr), (pl, p:pr)) ->
    CInfo (HM.insert (leafPP p) (pure $ B ()) $ fmap (pure . F) m)
          (map (pure . F) $ cl ++ cr)
          (pl ++ pr)
  _ -> error "PANIC: remove: bad column reference"

expand :: Int -> Int -> CompileInfo a -> CompileInfo (Var Int (Core a))
expand i n (CInfo m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, c:cr), (pl, p:pr)) ->
    CInfo (HM.insert (leafPP p) (pure $ F c) $ fmap (pure . F) m)
          (map (pure . F) cl ++ map (\j -> pure $ B j) [0..n-1] ++ map (pure . F) cr)
          (pl ++ map (\j -> p <> fieldPP j) [0..n-1] ++ pr)
  _ -> error "PANIC: expand: bad column reference"

instantiation :: CompileInfo a -> PatPath -> Core a
instantiation (CInfo pm cc cp) pp = case HM.lookup pp pm' of
  Nothing -> error "PANIC: instantiation: unknown pattern reference"
  Just c  -> c
 where
 pm' = HM.union pm . HM.fromList $ zip (map leafPP cp) cc

-- | Pattern matrices for compilation. The matrix is represented as a list
-- of columns. There is also an extra column representing the guards.
data PMatrix t a = PMatrix { _cols     :: [[Pattern t]]
                           , _guards   :: [Guard a]
                           , _bodies   :: [Scope PatPath Core a]
                           }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''PMatrix

promote :: (Functor f, Functor g) => f (g a) -> f (g (Var b (Core a)))
promote = fmap . fmap $ F . pure

defaultOn :: Int -> PMatrix t a -> PMatrix t (Var () (Core a))
defaultOn i (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PMatrix (map select $ ls ++ rs) (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

splitConOn :: Int -> Global -> PMatrix t a -> PMatrix t (Var Int (Core a))
splitConOn i g (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      p (ConP g' _, _) = g == g'
      p _              = False
      select c' = map snd . filter p $ zip c c'
      newcs = transpose [ ps' | ConP g' ps' <- c, g == g' ]
    in PMatrix (map select ls ++ newcs ++ map select rs)
               (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: splitConOn: bad column reference"

patternHeads :: [Pattern t] -> Set Global
patternHeads = setOf (traverse.patternHead)

-- | Uses a heuristic to select a column from a pattern matrix.
--
-- The current heuristic is longest constructor-headed column.
selectCol :: [[Pattern t]] -> Maybe Int
selectCol = fmap fst
          . maximumByOf folded (comparing snd)
          . zip [0..]
          . map (length . takeWhile (has patternHead))

-- | Compiles a pattern matrix together with a corresponding set of core
-- branches to a final Core value, which will be the decision tree version
-- of the pattern matrix.
compile :: MonadPComp m => CompileInfo a -> PMatrix t a -> m (Core a)
compile _  (PMatrix _  [] _)  = pure . HardCore $ Error "non-exhaustive pattern match."
compile ci pm@(PMatrix ps gs bs)
  | all (matchesTrivially . head) ps && has _Trivial (head gs) =
    pure . instantiate (instantiation ci) $ head bs
  | Just i <- selectCol ps = let
      col = ps !! i
      heads = patternHeads col
      dm = defaultOn i pm
    in do sig <- isSignature heads
          sms <- for (toListOf folded heads) $ \h -> do
                   n <- constructorArity h
                   (,) <$> constructorTag h
                       <*> (Scope <$> compile (expand i n ci) (splitConOn i h pm))
          Case ((ci^.colCores) !! i) (M.fromList sms) <$>
            if sig then pure Nothing else Just . Scope <$> compile (remove i ci) dm
  | otherwise = error "PANIC: pattern compile: No column selected."

