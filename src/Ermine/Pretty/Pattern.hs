{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD2
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Pattern
  ( prettyPattern
  , lambdaPatterns
  , prettyGuarded
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

newtype PP f a = PP { unPP :: State [String] (HashMap PatternPath String, f a) }

instance Functor f => Functor (PP f) where
  fmap f = PP . (fmap . fmap . fmap $ f) .  unPP

instance Applicative f => Applicative (PP f) where
  pure = PP . pure . pure . pure
  PP f <*> PP x = PP $ liftA2 (liftA2 (<*>)) f x

varPP :: Applicative f => PatternPath -> PP f Doc
varPP p = PP . state $ \(v:vars) -> ((HM.singleton p v, pure $ text v), vars)

runPP :: PP f a -> [String] -> (HashMap PatternPath String, f a)
runPP pp = evalState (unPP pp)

prettyPat' :: Applicative f
           => PatternPaths -> Pattern t -> Int -> (t -> Int -> f Doc) -> PP f Doc
prettyPat' path (SigP _t)   _    _  = varPP $ leafPP path
prettyPat' _    WildcardP   _    _  = pure $ text "_"
prettyPat' path (AsP p)     prec kt = h <$> varPP (leafPP path)
                                        <*> prettyPat' path p 12 kt
 where h l r = parensIf (prec > 12) $ l <> text "@" <> r
prettyPat' path (StrictP _) prec _  = h <$> varPP (leafPP path)
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' path (LazyP p)   prec kt = h <$> prettyPat' path p 13 kt
 where h l = parensIf (prec > 13) $ text "!" <> l
prettyPat' _    (LitP l)    _    _  = pure $ prettyLiteral l
prettyPat' path (ConP g ps) prec kt =
  h <$> traverse (\(i,o) -> prettyPat' (path <> fieldPP i) ?? 11 ?? kt $ o) (zip [0..] ps)
 where h l = parensIf (prec > 10) $ prettyGlobal g <+> hsep l
prettyPat' path (TupP ps)   _    kt =
  tupled <$> traverse (\(i,o) -> prettyPat' (path <> fieldPP i) ?? 0 ?? kt $ o) (zip [0..] ps)

prettyPattern :: Applicative f
              => Pattern t -> [String] -> Int
              -> (t -> Int -> f Doc) -> (HashMap PatternPath String, f Doc)
prettyPattern p vs prec tk = runPP (prettyPat' mempty p prec tk) vs

lambdaPatterns :: Applicative f
               => [Pattern t] -> [String] -> (t -> Int -> f Doc)
               -> (HashMap PatternPath String, f Doc)
lambdaPatterns ps vs tk =
  runPP (lsep <$> traverse (\(i,o) -> prettyPat' (argPP i) ?? 12 ?? tk $ o) (zip [0..] ps)) vs
 where lsep [] = empty ; lsep l = space <> hsep l

prettyGuarded :: Applicative f => Guarded tm -> Doc -> (tm -> f Doc) -> f Doc
prettyGuarded (Unguarded tm) goesTo k = (goesTo </>) <$> k tm
prettyGuarded (Guarded l)    goesTo k = align . sep <$> traverse (\(l', r) -> h <$> k l' <*> k r) l
 where
 h g b = text "|" <+> g <+> goesTo </> b

prettyAlt :: Applicative f
          => [String]
          -> (forall r. g r -> [String] -> Int -> (r -> Int -> f Doc) -> f Doc)
          -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> Alt t g v -> f Doc
prettyAlt vs kg kt kv (Alt pat gs) =
  (<+>) <$> fpd <*> prettyGuarded gs (text "->") (\(Scope e) -> kg e rest (-1) kv')
 where
 (bnd, fpd) = prettyPattern pat vs (-1) kt
 rest = drop (HM.size bnd) vs

 kv' (B p) _    = fromMaybe (error "PANIC: prettyAlt: Bad pattern variable reference") $
                    pure . text <$> HM.lookup p bnd
 kv' (F g) prec = kg g rest prec kv
