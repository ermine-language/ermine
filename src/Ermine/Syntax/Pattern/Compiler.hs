{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

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
  , PCompEnv(..)
  , MonadPComp(..)
  , isSignature
  , constructorTag
  , CompileInfo(..)
  , pathMap
  , colCores
  , colPaths
  , cols
  , guards
  , bodies
  , defaultOn
  , splitOn
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
import qualified Data.Set as S
import Data.Set.Lens
import Data.Traversable
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.Pattern

newtype PCompEnv = PCompEnv { signatures :: HashMap Global (HashMap Global Int) }
  deriving (Eq, Show)

class (Applicative m, Monad m) => MonadPComp m where
  askPComp :: m PCompEnv

instance MonadPComp ((->) PCompEnv) where
  askPComp = id

isSignature :: MonadPComp m => Set PatHead -> m Bool
isSignature ps = case preview folded ps of
  Nothing         -> pure False
  Just (TupH _)   -> pure True
  Just (ConH _ g) -> askPComp <&> \env -> case HM.lookup g $ signatures env of
    Nothing -> error $ "PANIC: isSignature: unknown constructor"
    Just hm -> iall (\g' _ -> S.member g' ns) hm
 where ns = S.map _name ps

constructorTag :: MonadPComp m => PatHead -> m Int
constructorTag (TupH _) = pure 0
constructorTag (ConH _ g) = askPComp <&> \env ->
  case HM.lookup g (signatures env) >>= HM.lookup g of
    Nothing -> error $ "PANIC: constructorTag: unknown constructor"
    Just i  -> i

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
  ((cl, _:cr), (pl, p:pr)) ->
    CInfo (HM.insert (leafPP p) (pure $ B 0) $ fmap (pure . F) m)
          (map (pure . F) cl ++ map (\j -> pure $ B j) [1..n] ++ map (pure . F) cr)
          (pl ++ map (\j -> p <> fieldPP j) [0..n-1] ++ pr)
  _ -> error "PANIC: expand: bad column reference"

instantiation :: CompileInfo a -> PatPath -> Core a
instantiation (CInfo pm cc cp) pp = case HM.lookup pp pm' of
  Nothing ->
    error $ "PANIC: instantiation: unknown pattern reference: " ++ show pp
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

bump :: CompileInfo a -> CompileInfo (Var Int (Core a))
bump (CInfo m cs ps) = CInfo (promote m) (promote cs) ps

defaultOn :: Int -> PMatrix t a -> PMatrix t (Var () (Core a))
defaultOn i (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      select c' = map snd . filter (matchesTrivially . fst) $ zip c c'
    in PMatrix (map select $ ls ++ rs) (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: defaultOn: bad column reference"

splitOn :: Int -> PatHead -> PMatrix t a -> PMatrix t (Var Int (Core a))
splitOn i hd (PMatrix ps gs cs)
  | (ls, c:rs) <- splitAt i ps = let
      con pat = traverseHead hd pat
      p (pat, _) = has con pat || matchesTrivially pat
      select c' = map snd . filter p $ zip c c'
      newcs = transpose $ c >>= \pat ->
        if | Just ps' <- preview con pat -> [ps']
           | matchesTrivially pat        -> [replicate (hd^.arity) WildcardP]
           | otherwise                   -> []
    in PMatrix (map select ls ++ newcs ++ map select rs)
               (promote $ select gs) (promote $ select cs)
  | otherwise = error "PANIC: splitOn: bad column reference"

peel :: PMatrix t a -> PMatrix t (Var Int (Core a))
peel (PMatrix ps (_:gs) (_:bs)) =
  PMatrix (map (drop 1) ps) (promote gs) (promote bs)
peel _ = error "PANIC: peel: malformed pattern matrix."

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
compile :: MonadPComp m => CompileInfo a -> PMatrix t a -> m (Core a)
compile _  (PMatrix _  [] _)  = pure . HardCore $ Error "non-exhaustive pattern match."
compile ci pm@(PMatrix ps gs bs)
  | all (matchesTrivially . head) ps = case head gs of
    Trivial -> pure . instantiate (instantiation ci) $ head bs
    Explicit e -> mk <$> compile (bump ci) (peel pm)
     where
     mk f = Case (instantiate (instantiation ci) e) ?? Nothing $
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
          Case ((ci^.colCores) !! i) (M.fromList sms) <$>
            if sig then pure Nothing else Just . Scope <$> compile (remove i ci) dm
  | otherwise = error "PANIC: pattern compile: No column selected."

