{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
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
import Bound.Var
import Control.Applicative
import Control.Lens
import Control.Monad.Writer
import Data.List (genericLength, genericSplitAt, genericIndex)
import Data.Word
import Ermine.Syntax
import Ermine.Syntax.Core
import Ermine.Unification.Sharing

optimize :: Core c -> Core c
optimize c = runIdentity . runSharing c $ optimize' 10 c

optimize' :: (Applicative m, MonadWriter Any m) => Int -> Core c -> m (Core c)
optimize' 0 c = return c
optimize' n c = do (c', Any b) <- listen $ suite c
                   if b then optimize' (n-1) c' else return c
 where
 suite = rewriteCoreDown lamlam
     >=> rewriteCore betaVar


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

-- | Beta reduces redexes like (\x.. -> e) v.. where v.. is all variables
betaVar :: forall c m. (Applicative m, MonadWriter Any m) => Core c -> m (Core c)
betaVar = open (const False)
 where
 inScope :: forall b d. (Core (Var b d) -> m (Core (Var b d)))
         -> Scope b Core d -> m (Scope b Core d)
 inScope f = fmap toScope . f . fromScope

 extend :: forall b v. (v -> Bool) -> (Var b v -> Bool)
 extend p v = has _B v || anyOf _F p v

 open :: forall v. (v -> Bool) -> Core v -> m (Core v)
 open p (Lam k e) = Lam k <$> inScope (open $ extend p) e
 open p (Case e bs d) = (Case e ?? d) <$> (traverse._2) (inScope . open $ extend p) bs
 open p c = collapse p [] c

 collapse :: forall v. (v -> Bool) -> [Core v] -> Core v -> m (Core v)
 collapse p stk (App f x)
   | anyOf var p x = collapse p (x:stk) f
 collapse p stk (Lam n body@(Scope body'))
   | len < n = do
     tell (Any True)
     let replace i | i < len   = F $ stk `genericIndex` i
                   | otherwise = B $ i - len
     return $ Lam (n - len) . Scope $ fmap (unvar replace F) body'
   | (args, stk') <- genericSplitAt n stk = do
     tell (Any True)
     collapse p stk' $ instantiate (genericIndex args) body
  where len = genericLength stk
 collapse _ stk c = return $ apps c stk
