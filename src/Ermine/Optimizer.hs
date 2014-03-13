{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
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
import Control.Applicative
import Control.Lens
import Control.Monad.Writer
import Data.Word
import Ermine.Syntax.Core
import Ermine.Unification.Sharing

optimize :: Core c -> Core c
optimize c = runIdentity . runSharing c $ rewriteCoreDown lamlam c

rewriteCoreDown :: forall m c. (Applicative m, MonadWriter Any m)
                => (forall d. Core d -> m (Core d)) -> Core c -> m (Core c)
rewriteCoreDown opt = go
 where
 go :: forall e. Core e -> m (Core e)
 go c = sharing c (opt c) >>= \case
   l@(Lam n e) -> sharing l $ Lam n <$> goS e
   d@(Data n l) -> sharing d $ Data n <$> traverse go l
   a@(App f x) -> sharing a $ App <$> go f <*> go x
   l@(Let d b) -> sharing l $ Let <$> sharing d (traverse goS d) <*> goS b
   l@(LamDict e) -> sharing l $ LamDict <$> goS e
   a@(AppDict f d) -> sharing a $ AppDict <$> go f <*> go d
   s@(Case e b d) ->
     sharing s $ Case <$> go e
                      <*> sharing b ((traverse._2) goS b)
                      <*> sharing d (traverse goS d)
   d@(Dict su sl) ->
     sharing d $ Dict <$> sharing su (traverse go su)
                      <*> sharing sl (traverse goS sl)
   x -> return x
 goS :: forall b e. Scope b Core e -> m (Scope b Core e)
 goS s = sharing s . fmap toScope . go . fromScope $ s

rewriteCore :: forall m c. (Applicative m, MonadWriter Any m)
            => (forall d. Core d -> m (Core d)) -> Core c -> m (Core c)
rewriteCore opt = go
 where
 go :: forall e. Core e -> m (Core e)
 go c = sharing c $ opt =<< case c of
   l@(Lam n e) -> sharing l $ Lam n <$> goS e
   d@(Data n l) -> sharing d $ Data n <$> traverse go l
   a@(App f x) -> sharing a $ App <$> go f <*> go x
   l@(Let d b) -> sharing l $ Let <$> sharing d (traverse goS d) <*> goS b
   l@(LamDict e) -> sharing l $ LamDict <$> goS e
   a@(AppDict f d) -> sharing a $ AppDict <$> go f <*> go d
   s@(Case e b d) ->
     sharing s $ Case <$> go e
                      <*> sharing b ((traverse._2) goS b)
                      <*> sharing d (traverse goS d)
   d@(Dict su sl) ->
     sharing d $ Dict <$> sharing su (traverse go su)
                      <*> sharing sl (traverse goS sl)
   _ -> return c
 goS :: forall b e. Scope b Core e -> m (Scope b Core e)
 goS s = sharing s . fmap toScope . go . fromScope $ s

-- | Turns \{x..} -> \{y..} -> ... into \{x.. y..} -> ...
lamlam :: forall c m. (Functor m, MonadWriter Any m) => Core c -> m (Core c)
lamlam (Lam k e) = slurp False k (fromScope e)
 where
 slurp :: forall e. Bool -> Word8 -> Core (Var Word8 e) -> m (Core e)
 slurp _ m (Lam n b) = slurp True (m + n) (instantiate (pure.B.(m+)) b)
 slurp b m c         = (Lam m $ toScope c) <$ tell (Any b)
lamlam c = return c

