{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
  , HasPMatrix(..)
  , PCompEnv(..)
  , MonadPComp(..)
  , isSignature
  , constructorTag
  , CompileInfo(..)
  , HasCompileInfo(..)
  , defaultOn
  , splitOn
  , compile
  , compileLambda
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
import qualified Data.Set as S
import Data.Set.Lens
import Data.Text as SText (pack)
import Data.Traversable
import Data.Word
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern

-- | The environment necessary to perform pattern compilation. We need two
-- pieces of information:
--
--   1) The signatures (full list of constructors) associated with each
--      particular constructor
--
--   2) A mapping from constructors to their @Core@ integer tag
--
-- This is accomplished via a map of maps. The outer map should take each
-- global to its associated signature, and signatures are represented as
-- maps from globals to integer tags.
newtype PCompEnv = PCompEnv { signatures :: HashMap Global (HashMap Global Word8) }
  deriving (Eq, Show)

-- | Monads that allow us to perform pattern compilation, by providing
-- a PCompEnv.
class (Applicative m, Monad m) => MonadPComp m where
  askPComp :: m PCompEnv

instance MonadPComp ((->) PCompEnv) where
  askPComp = id

-- | Determines whether a set of pattern heads constitutes a signature.
-- This is handled specially for tuples and literals, and relies on the
-- monad for data type constructors.
isSignature :: MonadPComp m => Set PatHead -> m Bool
isSignature ps = case preview folded ps of
  Nothing         -> pure False
  Just (TupH _)   -> pure True
  Just (ConH _ g) -> askPComp <&> \env -> case HM.lookup g $ signatures env of
    Nothing -> error $ "PANIC: isSignature: unknown constructor"
    Just hm -> iall (\g' _ -> S.member g' ns) hm
 where ns = S.map _name ps

-- | Looks up the constructor tag for a pattern head. For tuples this is
-- always 0, but constructors must consult the compilation environment.
constructorTag :: MonadPComp m => PatHead -> m Word8
constructorTag (TupH _) = pure 0
constructorTag (ConH _ g) = askPComp <&> \env ->
  case HM.lookup g (signatures env) >>= HM.lookup g of
    Nothing -> error $ "PANIC: constructorTag: unknown constructor"
    Just i  -> i

-- | Guard representation. Every row of a pattern matrix must have an
-- assocated guard, but if no guard was specified, then it will simply be
-- the trivial guard which allows everything.
data Guard c a = Trivial | Explicit (Scope PatPath c a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

makePrisms ''Guard

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
data CompileInfo c a = CInfo { _pathMap  :: HashMap PatPath (c a)
                             , _colCores :: [c a]
                             , _colPaths :: [PatPaths]
                             }

makeClassy ''CompileInfo

-- | @'remove' i@ removes the /i/th column of the 'CompileInfo'. This is for use
-- when compiling the default case for a column, and thus the resulting
-- 'CompileInfo' is lifted to be used in such a context.
remove :: Applicative c => Int -> CompileInfo c a -> CompileInfo c (Var () (c a))
remove i (CInfo m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, _:cr), (pl, p:pr)) ->
    CInfo (HM.insert (leafPP p) (pure $ B ()) $ fmap (pure . F) m)
          (map (pure . F) $ cl ++ cr)
          (pl ++ pr)
  _ -> error "PANIC: remove: bad column reference"

-- | @'expand' i n@ expands the /i/th column of a pattern into /n/ columns. This
-- is for use when compiling a constructor case of a pattern, and thus the
-- 'CompileInfo' returned is prepared to be used in such a case.
expand :: Applicative c
       => Int -> Word8 -> CompileInfo c a -> CompileInfo c (Var Word8 (c a))
expand i n (CInfo m ccs cps) = case (splitAt i ccs, splitAt i cps) of
  ((cl, _:cr), (pl, p:pr)) ->
    CInfo (HM.insert (leafPP p) (pure $ B 0) $ fmap (pure . F) m)
          (map (pure . F) cl ++ map (\j -> pure $ B j) [1..n] ++ map (pure . F) cr)
          (pl ++ map (\j -> p <> fieldPP j) [0..(fromIntegral n)-1] ++ pr)
  _ -> error "PANIC: expand: bad column reference"

instantiation :: CompileInfo c a -> PatPath -> c a
instantiation (CInfo pm cc cp) pp = fromMaybe
  (error $ "PANIC: instantiation: unknown pattern reference: " ++ show pp) (HM.lookup pp pm')
 where
 pm' = HM.union pm . HM.fromList $ zip (map leafPP cp) cc

-- | Pattern matrices for compilation. The matrix is represented as a list
-- of columns. There is also an extra column representing the guards.
data PMatrix t c a = PMatrix { _cols     :: [[Pattern t]]
                             , _guards   :: [Guard c a]
                             , _bodies   :: [Scope PatPath c a]
                             }
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeClassy ''PMatrix

-- | A helper function to make a more deeply nested core.
promote :: (Functor f, Functor g, Applicative c) => f (g a) -> f (g (Var b (c a)))
promote = fmap . fmap $ F . pure

-- | This lifts a CompileInfo to work under one additional scope. This is
-- necessary when we compile a guard, which eliminates a row, but leaves
-- all columns in place.
bump :: Applicative c => CompileInfo c a -> CompileInfo c (Var Word8 (c a))
bump (CInfo m cs ps) = CInfo (promote m) (promote cs) ps

-- | Computes the matrix that should be used recursively when defaulting on
-- the specified column.
defaultOn :: Applicative c => Int -> PMatrix t c a -> PMatrix t c (Var () (c a))
defaultOn i (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PMatrix (map select $ ls ++ rs) (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

-- | Computes the matrix that should be used recursively when defaulting on
-- the specified column, with the given pattern head.
splitOn :: Applicative c
        => Int -> PatHead -> PMatrix t c a -> PMatrix t c (Var Word8 (c a))
splitOn i hd (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      con pat = traverseHead hd pat
      p (pat, _) = has con pat || matchesTrivially pat
      select c' = map snd . filter p $ zip c c'
      newcs = transpose $ c >>= \pat ->
        if | Just ps' <- preview con pat -> [ps']
           | matchesTrivially pat        -> [replicate (fromIntegral $ hd^.arity) WildcardP]
           | otherwise                   -> []
    in PMatrix (map select ls ++ newcs ++ map select rs)
               (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: splitOn: bad column reference"

-- | Removes the first row of the pattern matrix, and prepares it to be
-- used in a context with a guard.
peel :: Applicative c => PMatrix t c a -> PMatrix t c (Var Word8 (c a))
peel (PMatrix ps (_:gs) (_:bs)) =
  PMatrix (map (drop 1) ps) (promote gs) (promote bs)
peel _ = error "PANIC: peel: malformed pattern matrix."

-- | Computes the set of heads of the given patterns.
patternHeads :: [Pattern t] -> Set PatHead
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

-- | Compiles a pattern matrix together with a corresponding set of core
-- branches to a final Core value, which will be the decision tree version
-- of the pattern matrix.
compile :: (MonadPComp m, Cored c) => CompileInfo c a -> PMatrix t c a -> m (c a)
compile _  (PMatrix _  [] _)  = pure . hardCore $ Error (SText.pack "non-exhaustive pattern match.")
compile ci pm@(PMatrix ps gs bs)
  | all (matchesTrivially . head) ps = case head gs of
    Trivial -> pure . instantiate (instantiation ci) $ head bs
    Explicit e -> mk <$> compile (bump ci) (peel pm)
     where
     mk f = caze (instantiate (instantiation ci) e) ?? Nothing $
              M.fromList
                [(1, (0, Scope . fmap (F . pure) $
                           instantiate (instantiation ci) $ head bs))
                ,(0, (0, Scope $ f))
                ]

  | Just i <- selectCol ps = let
      col = ps !! i
      heads = patternHeads col
      dm = defaultOn i pm
    in do sig <- isSignature heads
          sms <- for (toListOf folded heads) $ \h -> do
                   let n = h^.arity
                   (,) <$> constructorTag h
                       <*> ((,) n . Scope <$> compile (expand i n ci) (splitOn i h pm))
          caze ((ci^.colCores) !! i) (M.fromList sms) <$>
            if sig then pure Nothing else Just . Scope <$> compile (remove i ci) dm
  | otherwise = error "PANIC: pattern compile: No column selected."

compileLambda :: (MonadPComp m, Cored c)
              => [Pattern t] -> Scope PatPath c a -> m (Scope Word8 c a)
compileLambda ps body = compile ci pm
 where
 pm = PMatrix [ps] [Trivial] [hoistScope lift body]
 pps = zipWith (const . argPP) [0..] ps
 cs = zipWith (const . Scope . pure . B) [0..] ps
 ci = CInfo (HM.fromList $ zipWith ((,) . leafPP) pps cs) cs pps
