{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Ermine.Optimizer
  ( optimize
  , rewriteCore
  , rewriteCoreDown
  ) where

import Bound
import Bound.Var
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad.Writer
import Data.List (genericLength, genericSplitAt, genericIndex)
import Data.Traversable (sequenceA)
import Data.Word
import Ermine.Syntax
import Ermine.Syntax.Core
import Ermine.Syntax.Scope
import Ermine.Unification.Sharing

-- | Optimize core expressions by alternating between the different optimization passes several times.
optimize :: Core c -> Core c
optimize c = runIdentity . runSharing c $ optimize' 10 c

optimize' :: (Applicative m, MonadWriter Any m) => Int -> Core c -> m (Core c)
optimize' 0 c = return c
optimize' n c = do (c', Any b) <- listen $ suite c
                   if b then optimize' (n-1) c' else return c
 where
 suite = rewriteCoreDown lamlam
     >=> rewriteCore betaVar
     >=> rewriteCoreDown specCase
     >=> rewriteCore etaDict

rewriteCoreDown :: forall m c. (Applicative m, MonadWriter Any m)
                => (forall d. Core d -> m (Core d)) -> Core c -> m (Core c)
rewriteCoreDown opt = go
 where
 go :: forall e. Core e -> m (Core e)
 go c = sharing c (opt c) >>= \ xs -> case xs of
   l@(Lam n e) -> sharing l $ Lam n <$> goS e
   d@(Data n u g l) -> sharing d $ Data n u g <$> traverse go l
   a@(App f x) -> sharing a $ App <$> go f <*> go x
   l@(Let d b) -> sharing l $ Let <$> sharing d (traverse goS d) <*> goS b
   l@(LamDict n e) -> sharing l $ LamDict n <$> goS e
   a@(AppDict f d) -> sharing a $ AppDict <$> go f <*> go d
   l@(LamHash n e) -> sharing l $ LamHash n <$> goS e
   a@(AppHash f d) -> sharing a $ AppHash <$> go f <*> go d
   s@(Case e b d) ->
     sharing s $ Case <$> go e
                      <*> sharing b ((traverse.matchBody) goS b)
                      <*> sharing d (traverse goS d)
   s@(CaseHash e b d) ->
     sharing s $ CaseHash <$> go e
                          <*> sharing b (traverse go b)
                          <*> sharing d (traverse go d)
   d@(Dict su sl) ->
     sharing d $ Dict <$> sharing su (traverse go su)
                      <*> sharing sl (traverse goS sl)
   x -> return x
 goS :: forall b e. Scope b Core e -> m (Scope b Core e)
 goS s = sharing s . inScope go $ s

rewriteCore :: forall m c. (Applicative m, MonadWriter Any m)
            => (forall d. Core d -> m (Core d)) -> Core c -> m (Core c)
rewriteCore opt = go
 where
 go :: forall e. Core e -> m (Core e)
 go c = sharing c $ opt =<< case c of
   l@(Lam n e) -> sharing l $ Lam n <$> goS e
   d@(Data n u g l) -> sharing d $ Data n u g <$> traverse go l
   a@(App f x) -> sharing a $ App <$> go f <*> go x
   l@(Let d b) -> sharing l $ Let <$> sharing d (traverse goS d) <*> goS b
   l@(LamDict n e) -> sharing l $ LamDict n <$> goS e
   a@(AppDict f d) -> sharing a $ AppDict <$> go f <*> go d
   l@(LamHash n e) -> sharing l $ LamHash n <$> goS e
   a@(AppHash f d) -> sharing a $ AppHash <$> go f <*> go d
   s@(Case e b d) ->
     sharing s $ Case <$> go e
                      <*> sharing b ((traverse.matchBody) goS b)
                      <*> sharing d (traverse goS d)
   s@(CaseHash e b d) ->
     sharing s $ CaseHash <$> go e
                          <*> sharing b (traverse go b)
                          <*> sharing d (traverse go d)
   d@(Dict su sl) ->
     sharing d $ Dict <$> sharing su (traverse go su)
                      <*> sharing sl (traverse goS sl)
   _ -> return c
 goS :: forall b e. Scope b Core e -> m (Scope b Core e)
 goS s = sharing s . inScope go $ s

-- | Turns @\{x..} -> \{y..} -> ...@ into @\{x.. y..} -> ...@
lamlam :: forall c m. (Functor m, MonadWriter Any m) => Core c -> m (Core c)
lamlam (Lam k e) = slurp False k (fromScope e)
 where
 slurp :: forall e. Bool -> Word8 -> Core (Var Word8 e) -> m (Core e)
 slurp _ m (Lam n b) = slurp True (m + n) (instantiate (pure.B.(m+)) b)
 slurp b m c         = Lam m (toScope c) <$ tell (Any b)
lamlam c = return c

-- | 'LamDict' is strict, so η-reduction for it is sound. η-reduce.
--
-- Todo: generalize this to larger lambdas
etaDict :: forall c m. (Functor m, MonadWriter Any m) => Core c -> m (Core c)
etaDict c@(LamDict 1 (Scope (AppDict f (Var (B 0))))) = case sequenceA f of
  B _ -> return c
  F g -> join g <$ tell (Any True)
etaDict c = return c

-- | β-reduces redexes like @(\x.. -> e) v..@ where @v..@ is all variables
betaVar :: forall c m. (Applicative m, MonadWriter Any m) => Core c -> m (Core c)
betaVar = collapse []
 where
 collapse stk (App f x)
   | has _Var x || has _Slot x || has _Super x || has _Lit x = collapse (x:stk) f
 collapse stk (Lam n body@(Scope body'))
   | len < n = do
     tell (Any True)
     let replace i | i < len   = F $ stk `genericIndex` i
                   | otherwise = B $ i - len
     return $ Lam (n - len) . Scope $ fmap (unvar replace F) body'
   | (args, stk') <- genericSplitAt n stk = do
     tell (Any True)
     collapse stk' $ instantiate (genericIndex args) body
  where len = genericLength stk
 collapse stk c = return $ apps c stk

-- | Specializes a case expression to a known constructor.
specCase :: forall m c. (Applicative m, MonadWriter Any m) => Core c -> m (Core c)
specCase (Case dat@(Data n 0 g as) bs d)
  -- TODO: Use a LamHash around this for all the unboxed arguments, and AppHash each to an argument.
  | Just (Match arity 0 _ body) <- bs ^. at n =
    Let ((Scope . Data n 0 g $ pure . B <$> [1..fromIntegral arity]) : map lift as)
        (mapBound fromIntegral body)
      <$ tell (Any True)
  | Just e <- d = Let [lift dat] (mapBound (const 0) e) <$ tell (Any True)
  | otherwise = error "PANIC: non-exhaustive case with no default"
specCase c = pure c
