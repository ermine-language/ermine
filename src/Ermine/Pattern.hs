{-# LANGUAGE MultiWayIf #-}
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
module Ermine.Pattern
  (
  -- * Monad
    module Ermine.Pattern.Env
  -- * Pattern Matrix
  , module Ermine.Pattern.Matrix
  -- * Matching
  , Matching(..)
  , HasMatching(..)
  -- * Compilation
  , compile
  , compileBinding
  , compileLambda
  , compileCase
  ) where

import Prelude hiding (all)

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad.Trans (lift)
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.List (transpose)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import Data.Set.Lens
import Data.Text as SText (pack)
import Data.Traversable
import Data.Word
import Ermine.Pattern.Env
import Ermine.Pattern.Matrix
import Ermine.Syntax.Core
import Ermine.Syntax.Pattern
import Ermine.Syntax.Scope
import Ermine.Syntax.Term

-- | Additional information needed for pattern compilation that does not
-- really belong in the pattern matrix.
--
-- The 'pathMap' stores information for resolving previous levels of the
-- pattern to appropriate cores for the current context.
--
-- 'colCores' contains core terms that refer to each of the columns of the
-- matrix in the current context.
--
-- 'colPaths' contains the paths into the original pattern that correspond to
-- each of the current columns.
data Matching c a = Matching
  { _pathMap  :: HashMap PatternPath (c a)
  , _colCores :: [c a]
  , _colPaths :: [PatternPaths]
  }

makeClassy ''Matching

-- | @'remove' i@ removes the /i/th column of the 'Matching'. This is for use
-- when compiling the default case for a column, and thus the resulting
-- 'Matching' is lifted to be used in such a context.
remove :: Applicative c => Int -> Matching c a -> Matching c (Var () (c a))
remove i (Matching m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, _:cr), (pl, p:pr)) ->
    Matching (HM.insert (leafPP p) (pure $ B ()) $ fmap (pure . F) m)
          (map (pure . F) $ cl ++ cr)
          (pl ++ pr)
  _ -> error "PANIC: remove: bad column reference"

-- | @'expand' i n@ expands the /i/th column of a pattern into /n/ columns. This
-- is for use when compiling a constructor case of a pattern, and thus the
-- 'Matching' returned is prepared to be used in such a case.
expand :: Applicative c
       => Int -> Word8 -> Matching c a -> Matching c (Var Word8 (c a))
expand i n (Matching m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, _:cr), (pl, p:pr)) ->
    Matching (HM.insert (leafPP p) (pure $ B 0) $ fmap (pure . F) m)
          (map (pure . F) cl ++ map (\j -> pure $ B j) [1..n] ++ map (pure . F) cr)
          (pl ++ map (\j -> p <> fieldPP j) [0..(fromIntegral n)-1] ++ pr)
  _ -> error "PANIC: expand: bad column reference"

instantiation :: Matching c a -> PatternPath -> c a
instantiation (Matching pm cc cp) pp = fromMaybe
  (error $ "PANIC: instantiation: unknown pattern reference: " ++ show pp) (HM.lookup pp pm')
 where
 pm' = HM.union pm . HM.fromList $ zip (map leafPP cp) cc

-- | A helper function to make a more deeply nested core.
promote :: (Functor f, Functor g, Applicative c) => f (g a) -> f (g (Var b (c a)))
promote = fmap . fmap $ F . pure

-- | This lifts a Matching to work under one additional scope. This is
-- necessary when we compile a guard, which eliminates a row, but leaves
-- all columns in place.
bumpM :: Applicative c => Matching c a -> Matching c (Var b (c a))
bumpM (Matching m cs ps) = Matching (promote m) (promote cs) ps

hbumpM :: Monad c => Matching c a -> Matching (Scope b c) a
hbumpM (Matching m cs ps) = Matching (lift <$> m) (lift <$> cs) ps

bumpPM :: Applicative c => PatternMatrix t c a -> PatternMatrix t c (Var b (c a))
bumpPM (PatternMatrix ps bs) = PatternMatrix ps (promote $ bs)

hbumpPM :: (Functor c, Monad c) => PatternMatrix t c a -> PatternMatrix t (Scope b c) a
hbumpPM (PatternMatrix ps bs) = PatternMatrix ps (hoistClaused lift <$> bs)

-- | Computes the set of heads of the given patterns.
patternHeads :: [Pattern t] -> Set PatternHead
patternHeads = setOf (traverse.patternHead)

-- | Uses a heuristic to select a column from a pattern matrix.
--
-- The current heuristic selects a column with the longest initial segment
-- of patterns that force evaluation.
selectCol :: [[Pattern t]] -> Maybe Int
selectCol = fmap fst
          . maximumByOf folded (comparing snd)
          . zip [0..]
          . map (length . takeWhile forces)

compileClaused :: (MonadPattern m, Cored c)
               => Matching c a
               -> PatternMatrix t c a
               -> Claused c a
               -> m (c a)
compileClaused m pm (Raw gbs) = compileGuards m pm gbs
compileClaused m pm (Localized ws gbs) =
  letrec (inst <$> ws) <$> compileGuards m' pm' gbs
 where
 m' = hbumpM m
 pm' = hbumpPM pm
 inst = instantiate (instantiation m')

compileGuards :: (MonadPattern m, Cored c)
              => Matching c a
              -> PatternMatrix t c a
              -> Guarded (Scope PatternPath c a)
              -> m (c a)
-- In the unguarded case, we can terminate immediately, because all patterns
-- leading up to here matched trivially.
compileGuards m _  (Unguarded body) = pure $ instantiate (instantiation m) body
compileGuards m pm (Guarded bods) = compileManyGuards m pm bods

compileManyGuards :: (MonadPattern m, Cored c)
                  => Matching c a
                  -> PatternMatrix t c a
                  -> [(Scope PatternPath c a, Scope PatternPath c a)]
                  -> m (c a)
compileManyGuards m pm [] = compile m pm
-- TODO: check for a trivial guard here
compileManyGuards m pm ((g,b):gbs) =
  compileManyGuards
    (bumpM m)
    (bumpPM pm)
    (over (traverse.both) (fmap $ F . pure) gbs) <&> \f ->
  caze (inst g) ?? Nothing $
    M.fromList
      [(1, (0, Scope . fmap (F . pure) $ inst b))
      ,(0, (0, Scope $ f))
      ]
 where inst = instantiate (instantiation m)


-- | Compiles a pattern matrix together with a corresponding set of core
-- branches to a final Core value, which will be the decision tree version
-- of the pattern matrix.
compile :: (MonadPattern m, Cored c) => Matching c a -> PatternMatrix t c a -> m (c a)
compile _  (PatternMatrix _  [])  = pure . hardCore $ Error (SText.pack "non-exhaustive pattern match.")
compile m pm@(PatternMatrix ps (b:bs))
  | all (matchesTrivially . head) ps =
     compileClaused m (PatternMatrix (drop 1 <$> ps) bs) b

  | Just i <- selectCol ps = let
      col = ps !! i
      heads = patternHeads col
      dm = defaultOn i pm
    in do sig <- isSignature heads
          sms <- for (toListOf folded heads) $ \h -> do
                   let n = h^.arity
                   (,) <$> constructorTag h
                       <*> ((,) n . Scope <$> compile (expand i n m) (splitOn i h pm))
          caze ((m^.colCores) !! i) (M.fromList sms) <$>
            if sig then pure Nothing else Just . Scope <$> compile (remove i m) dm
  | otherwise = error "PANIC: pattern compile: No column selected."

bodyVar :: BodyBound -> Var (Var PatternPath Word32) Word32
bodyVar (BodyDecl w) = F w
bodyVar (BodyPat p) = B (B p)
bodyVar (BodyWhere w) = B (F w)

bodyVar_ :: BodyBound -> Var PatternPath Word32
bodyVar_ (BodyDecl w) = F w
bodyVar_ (BodyPat p) = B p
bodyVar_ (BodyWhere _) = error "panic: reference to non-existent where clause detected"

whereVar :: WhereBound -> Var PatternPath Word32
whereVar (WhereDecl w) = F w
whereVar (WherePat p) = B p

compileBinding
  :: (MonadPattern m, Cored c)
  => [[Pattern t]] -> [Guarded (Scope BodyBound c a)] -> [[Scope (Var WhereBound Word32) c a]] -> m (Scope Word32 c a)
compileBinding ps gds ws = lambda (fromIntegral $ length $ head ps) <$> compile m pm where
  clause g [] = Raw (hoistScope lift . splitScope . mapBound bodyVar_ <$> g)
  clause g w = Localized (splitScope . mapBound whereVar . hoistScope lift . splitScope <$> w)
                         (splitScope . hoistScope lift . splitScope . mapBound bodyVar <$> g)
  pm = PatternMatrix (transpose ps) (zipWith clause gds ws)
  pps = zipWith (const . argPP) [0..] (head ps)
  cs = zipWith (const . Scope . pure . B) [(0 :: Word8)..] (head ps)
  m = Matching (HM.fromList $ zipWith ((,) . leafPP) pps cs) cs pps

compileLambda
  :: (MonadPattern m, Cored c)
  => [Pattern t] -> Scope PatternPath c a -> m (Scope Word8 c a)
compileLambda ps body = compile m pm where
 pm = PatternMatrix (map return ps) [Raw . Unguarded $ hoistScope lift body]
 pps = zipWith (const . argPP) [0..] ps
 cs = zipWith (const . Scope . pure . B) [0..] ps
 m = Matching (HM.fromList $ zipWith ((,) . leafPP) pps cs) cs pps

compileCase
  :: (MonadPattern m, Cored c)
  => [Pattern t] -> c a -> [Guarded (Scope PatternPath c a)] -> m (c a)
compileCase ps disc bs = compile m pm where
 pm = PatternMatrix [ps] (Raw <$> bs)
 m = Matching HM.empty [disc] [mempty]
