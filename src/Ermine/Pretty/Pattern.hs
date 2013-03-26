
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Pretty.Pattern ( prettyPattern
                             , lambdaPatterns
                             ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Bifunctor
import Data.Semigroup
import Ermine.Pretty
import Ermine.Pretty.Global
import Ermine.Pretty.Literal
import Ermine.Syntax.Pat

newtype PP f a = PP { unPP :: State [String] (Sum Int, f a) }

instance Functor f => Functor (PP f) where
  fmap f = PP . (fmap . fmap . fmap $ f) .  unPP

instance Applicative f => Applicative (PP f) where
  pure = PP . pure . pure . pure
  PP f <*> PP x = PP $ liftA2 (liftA2 (<*>)) f x

-- liftPP :: f a -> PP f a
-- liftPP = PP . pure . pure

tellPP :: Applicative f => Int -> PP f ()
tellPP i = PP $ pure (Sum i, pure ())

statePP :: Applicative f => ([String] -> (a, [String])) -> PP f a
statePP = PP . fmap (pure . pure) . state

varPP :: Applicative f => PP f Doc
varPP = tellPP 1 *> statePP (\(v:vars) -> (text v, vars))

runPP :: PP f a -> [String] -> (Int, f a)
runPP pp = first getSum . evalState (unPP pp)

prettyPat' :: Applicative f
           => Pat t -> Int -> (t -> Int -> f Doc) -> PP f Doc
prettyPat' (SigP _t)   _    _  = varPP
prettyPat' WildcardP   _    _  = pure $ text "_"
prettyPat' (AsP p)     prec kt = h <$> varPP <*> prettyPat' p 12 kt
 where h l r = parensIf (prec > 12) $ l <> text "@" <> r
prettyPat' (StrictP p) prec kt = h <$> prettyPat' p 13 kt
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' (LazyP p)   prec kt = h <$> prettyPat' p 13 kt
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' (LitP l)    _    _  = pure $ prettyLiteral l
prettyPat' (ConP g ps) prec kt = h <$> traverse (prettyPat' ?? 11 ?? kt) ps
 where h l = parensIf (prec > 10) $ prettyGlobal g <+> hsep l
prettyPat' (TupP ps)   _    kt = tupled <$> traverse (prettyPat' ?? 0 ?? kt) ps

prettyPattern :: Applicative f
              => Pat t -> [String] -> Int
              -> (t -> Int -> f Doc) -> (Int, f Doc)
prettyPattern p vs prec tk = runPP (prettyPat' p prec tk) vs

lambdaPatterns :: Applicative f
               => [Pat t] -> [String] -> (t -> Int -> f Doc) -> (Int, f Doc)
lambdaPatterns ps vs tk =
  runPP (hsep <$> traverse (prettyPat' ?? 1000 ?? tk) ps) vs
