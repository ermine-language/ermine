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
module Ermine.Match
  ( Claused(..)
  , PatternMatrix(..)
  , HasPatternMatrix(..)
  , MatchEnv(..)
  , dummyMatchEnv
  , MonadMatch(..)
  , isSignature
  , constructorTag
  , Matching(..)
  , HasMatching(..)
  , defaultOn
  , splitOn
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
import qualified Data.Set as S
import Data.Set.Lens
import Data.Text as SText (pack)
import Data.Traversable
import Data.Word
import Ermine.Syntax.Core
import Ermine.Syntax.Global
import Ermine.Syntax.ModuleName
import Ermine.Syntax.Pattern
import Ermine.Syntax.Scope
import Ermine.Syntax.Term

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
newtype MatchEnv = MatchEnv { signatures :: HashMap Global (HashMap Global Word8) }
  deriving (Eq, Show)

dummyMatchEnv :: MatchEnv
dummyMatchEnv = MatchEnv $ HM.singleton n (HM.singleton n 0) where
  n = glob Idfix (mkModuleName (pack "ermine") (pack "Ermine")) (pack "E")


-- | Monads that allow us to perform pattern compilation, by providing
-- a MatchEnv.
class (Applicative m, Monad m) => MonadMatch m where
  askMatch :: m MatchEnv

instance MonadMatch ((->) MatchEnv) where
  askMatch = id

-- | Determines whether a set of pattern heads constitutes a signature.
-- This is handled specially for tuples and literals, and relies on the
-- monad for data type constructors.
isSignature :: MonadMatch m => Set PatternHead -> m Bool
isSignature ps = case preview folded ps of
  Nothing         -> pure False
  Just (TupH _)   -> pure True
  Just (ConH _ g) -> askMatch <&> \env -> case HM.lookup g $ signatures env of
    Nothing -> error $ "PANIC: isSignature: unknown constructor"
    Just hm -> iall (\g' _ -> S.member g' ns) hm
 where ns = S.map _name ps

-- | Looks up the constructor tag for a pattern head. For tuples this is
-- always 0, but constructors must consult the compilation environment.
constructorTag :: MonadMatch m => PatternHead -> m Word8
constructorTag (TupH _) = pure 0
constructorTag (ConH _ g) = askMatch <&> \env ->
  case HM.lookup g (signatures env) >>= HM.lookup g of
    Nothing -> error $ "PANIC: constructorTag: unknown constructor"
    Just i  -> i

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

-- | This lifts a Matching to work under one additional scope. This is
-- necessary when we compile a guard, which eliminates a row, but leaves
-- all columns in place.
bumpCI :: Applicative c => Matching c a -> Matching c (Var b (c a))
bumpCI (Matching m cs ps) = Matching (promote m) (promote cs) ps

hbumpCI :: Monad c => Matching c a -> Matching (Scope b c) a
hbumpCI (Matching m cs ps) = Matching (lift <$> m) (lift <$> cs) ps

bumpPM :: Applicative c => PatternMatrix t c a -> PatternMatrix t c (Var b (c a))
bumpPM (PatternMatrix ps bs) = PatternMatrix ps (promote $ bs)

hbumpPM :: (Functor c, Monad c) => PatternMatrix t c a -> PatternMatrix t (Scope b c) a
hbumpPM (PatternMatrix ps bs) = PatternMatrix ps (hoistClaused lift <$> bs)

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
      prune (StrictP r) = prune r
      prune r = r
      p (pat, _) = has con (prune pat) || matchesTrivially pat
      select c' = map snd . filter p $ zip c c'
      newcs = transpose $ c >>= \pat ->
        if | Just ps' <- preview con pat -> [ps']
           | matchesTrivially pat        -> [replicate (fromIntegral $ hd^.arity) WildcardP]
           | otherwise                   -> []
    in PatternMatrix (map select ls ++ newcs ++ map select rs)
               (promote $ select cs)
  | otherwise = error "PANIC: splitOn: bad column reference"

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

compileClaused :: (MonadMatch m, Cored c)
               => Matching c a
               -> PatternMatrix t c a
               -> Claused c a
               -> m (c a)
compileClaused ci pm (Raw gbs) = compileGuards ci pm gbs
compileClaused ci pm (Localized ws gbs) =
  letrec (inst <$> ws) <$> compileGuards ci' pm' gbs
 where
 ci' = hbumpCI ci
 pm' = hbumpPM pm
 inst = instantiate (instantiation ci')

compileGuards :: (MonadMatch m, Cored c)
              => Matching c a
              -> PatternMatrix t c a
              -> Guarded (Scope PatternPath c a)
              -> m (c a)
-- In the unguarded case, we can terminate immediately, because all patterns
-- leading up to here matched trivially.
compileGuards ci _  (Unguarded body) = pure $ instantiate (instantiation ci) body
compileGuards ci pm (Guarded bods) = compileManyGuards ci pm bods

compileManyGuards :: (MonadMatch m, Cored c)
                  => Matching c a
                  -> PatternMatrix t c a
                  -> [(Scope PatternPath c a, Scope PatternPath c a)]
                  -> m (c a)
compileManyGuards ci pm [] = compile ci pm
-- TODO: check for a trivial guard here
compileManyGuards ci pm ((g,b):gbs) =
  compileManyGuards
    (bumpCI ci)
    (bumpPM pm)
    (over (traverse.both) (fmap $ F . pure) gbs) <&> \f ->
  caze (inst g) ?? Nothing $
    M.fromList
      [(1, (0, Scope . fmap (F . pure) $ inst b))
      ,(0, (0, Scope $ f))
      ]
 where inst = instantiate (instantiation ci)


-- | Compiles a pattern matrix together with a corresponding set of core
-- branches to a final Core value, which will be the decision tree version
-- of the pattern matrix.
compile :: (MonadMatch m, Cored c) => Matching c a -> PatternMatrix t c a -> m (c a)
compile _  (PatternMatrix _  [])  = pure . hardCore $ Error (SText.pack "non-exhaustive pattern match.")
compile ci pm@(PatternMatrix ps (b:bs))
  | all (matchesTrivially . head) ps =
     compileClaused ci (PatternMatrix (drop 1 <$> ps) bs) b

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
  :: (MonadMatch m, Cored c)
  => [[Pattern t]] -> [Guarded (Scope BodyBound c a)] -> [[Scope (Var WhereBound Word32) c a]] -> m (Scope Word32 c a)
compileBinding ps gds ws = lambda (fromIntegral $ length $ head ps) <$> compile ci pm where
  clause g [] = Raw (hoistScope lift . splitScope . mapBound bodyVar_ <$> g)
  clause g w = Localized (splitScope . mapBound whereVar . hoistScope lift . splitScope <$> w)
                         (splitScope . hoistScope lift . splitScope . mapBound bodyVar <$> g)
  pm = PatternMatrix (transpose ps) (zipWith clause gds ws)
  pps = zipWith (const . argPP) [0..] (head ps)
  cs = zipWith (const . Scope . pure . B) [(0 :: Word8)..] (head ps)
  ci = Matching (HM.fromList $ zipWith ((,) . leafPP) pps cs) cs pps

compileLambda
  :: (MonadMatch m, Cored c)
  => [Pattern t] -> Scope PatternPath c a -> m (Scope Word8 c a)
compileLambda ps body = compile ci pm where
 pm = PatternMatrix (map return ps) [Raw . Unguarded $ hoistScope lift body]
 pps = zipWith (const . argPP) [0..] ps
 cs = zipWith (const . Scope . pure . B) [0..] ps
 ci = Matching (HM.fromList $ zipWith ((,) . leafPP) pps cs) cs pps

compileCase
  :: (MonadMatch m, Cored c)
  => [Pattern t] -> c a -> [Guarded (Scope PatternPath c a)] -> m (c a)
compileCase ps disc bs = compile ci pm where
 pm = PatternMatrix [ps] (Raw <$> bs)
 ci = Matching HM.empty [disc] [mempty]
