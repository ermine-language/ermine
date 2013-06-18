{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Pattern
  ( prettyPattern
  , lambdaPatterns
  , prettyAlt
  ) where

import Bound
import Control.Applicative hiding (empty)
import Control.Lens
import Control.Monad.State
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Ermine.Pretty
import Ermine.Pretty.Global
import Ermine.Pretty.Literal
import Ermine.Syntax.Pattern

newtype PP f a = PP { unPP :: State [String] (HashMap PatPath String, f a) }

instance Functor f => Functor (PP f) where
  fmap f = PP . (fmap . fmap . fmap $ f) .  unPP

instance Applicative f => Applicative (PP f) where
  pure = PP . pure . pure . pure
  PP f <*> PP x = PP $ liftA2 (liftA2 (<*>)) f x

varPP :: Applicative f => PatPath -> PP f Doc
varPP p = PP . state $ \(v:vars) -> ((HM.singleton p v, pure $ text v), vars)

runPP :: PP f a -> [String] -> (HashMap PatPath String, f a)
runPP pp = evalState (unPP pp)

prettyPat' :: Applicative f
           => PatPaths -> Pattern t -> Int -> (t -> Int -> f Doc) -> PP f Doc
prettyPat' path (SigP _t)   _    _  = varPP $ leafPP path
prettyPat' _    WildcardP   _    _  = pure $ text "_"
prettyPat' path (AsP p)     prec kt = h <$> varPP (leafPP path)
                                        <*> prettyPat' (path <> inPP) p 12 kt
 where h l r = parensIf (prec > 12) $ l <> text "@" <> r
prettyPat' path (StrictP p) prec kt = h <$> prettyPat' (path <> inPP) p 13 kt
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' path (LazyP p)   prec kt = h <$> prettyPat' (path <> inPP) p 13 kt
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' _    (LitP l)    _    _  = pure $ prettyLiteral l
prettyPat' path (ConP g ps) prec kt =
  h <$> itraverse (\i -> prettyPat' (path <> fieldPP i) ?? 11 ?? kt) ps
 where h l = parensIf (prec > 10) $ prettyGlobal g <+> hsep l
prettyPat' path (TupP ps)   _    kt =
  tupled <$> itraverse (\i -> prettyPat' (path <> fieldPP i) ?? 0 ?? kt) ps

prettyPattern :: Applicative f
              => Pattern t -> [String] -> Int
              -> (t -> Int -> f Doc) -> (HashMap PatPath String, f Doc)
prettyPattern p vs prec tk = runPP (prettyPat' mempty p prec tk) vs

lambdaPatterns :: Applicative f
               => [Pattern t] -> [String] -> (t -> Int -> f Doc)
               -> (HashMap PatPath String, f Doc)
lambdaPatterns ps vs tk =
  runPP (lsep <$> itraverse (\i -> prettyPat' (argPP i) ?? 1000 ?? tk) ps) vs
 where lsep [] = empty ; lsep l = space <> hsep l

prettyAlt :: Applicative f
          => [String]
          -> (forall r. g r -> [String] -> Int -> (r -> Int -> f Doc) -> f Doc)
          -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> Alt t g v -> f Doc
prettyAlt vs kg kt kv (Alt pat (Scope e)) = h <$> fpd <*> kg e rest (-1) kv'
 where
 (bnd, fpd) = prettyPattern pat vs (-1) kt
 rest = drop (HM.size bnd) vs

 kv' (B p) _    = fromMaybe (error "PANIC: prettyAlt: Bad pattern variable reference") $
                    pure . text <$> HM.lookup p bnd
 kv' (F g) prec = kg g rest prec kv

 h pd ed = pd <+> text "->" <+> ed
