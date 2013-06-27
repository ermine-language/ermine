--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Term
  ( prettyHardTerm
  , prettyTerm
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Bifunctor
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Ermine.Pretty
import Ermine.Pretty.Global
import Ermine.Pretty.Literal
import Ermine.Pretty.Pattern
import Ermine.Syntax.Term

prettyHardTerm :: HardTerm -> Doc
prettyHardTerm (Lit l)       = prettyLiteral l
prettyHardTerm (DataCon g _) = prettyGlobal g
prettyHardTerm (Tuple n)     = parens . text $ replicate (n-1) ','
prettyHardTerm Hole          = text "?{}"

prettyTerm :: Applicative f
           => Term t v -> [String] -> Int
           -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> f Doc
prettyTerm (HardTerm h)   _    _    _  _  = pure $ prettyHardTerm h
prettyTerm (Loc _ e)      vars prec kt kv = prettyTerm e vars prec kt kv
prettyTerm (Remember _ e) vars prec kt kv = prettyTerm e vars prec kt kv
prettyTerm (Var v)        _    prec _  kv = kv v prec
-- TODO: operators
prettyTerm (App f x) vars prec kt kv =
  (\df dx -> parensIf (prec > 10) $ df <+> dx)
    <$> prettyTerm f vars 10 kt kv
    <*> prettyTerm x vars 11 kt kv
prettyTerm (Sig tm ty) vars prec kt kv =
  (\dm dy -> parensIf (prec >= 0) $ dm <+> text ":" <+> dy)
    <$> prettyTerm tm vars 0 kt kv
    <*> kt ty (-1)
prettyTerm (Lam ps (Scope e)) vars prec kt kv =
  h <$> fpd <*> prettyTerm e vars' (-1) kt kv'
 where
 (bnd, fpd) = lambdaPatterns ps vars kt
 (_, vars') = splitAt (HM.size bnd) vars
 h pd bd = parensIf (prec >= 0) $ pd <+> text "->" <+> bd
 kv' (B b) _ = fromMaybe (error "PANIC: prettyTerm: invlaid pattern variable reference") $
                 pure . text <$> HM.lookup b bnd
 kv' (F t) p = prettyTerm t vars' p kt kv
prettyTerm (Case d alts)  vars prec kt kv =
  h <$> prettyTerm d vars (-1) kt kv
    <*> traverse (prettyAlt vars (\tm vs pr kv' -> prettyTerm tm vs pr kt kv') kt kv) alts
 where
 h dd cs = parensIf (prec > 9) $
             text "case" <+> dd <+> text "of" <> nest 2 (group $ line <> block cs)
prettyTerm (Let bs e)     vars prec kt kv =
  h <$> prettyBindings (zip dvs bs) dvs rest kt kv
    <*> prettyTerm (unscope e) rest (-1) kt kv'
 where
 (dvs, rest) = first (map text) $ splitAt (length bs) vars
 kv' (B i) _  = pure $ dvs !! i
 kv' (F t) pr = prettyTerm t rest pr kt kv

 h bd ed = parensIf (prec > 9) $ text "let" <+> align bd </> text "in" </> ed

prettyBinding :: Applicative f
              => Doc -> Binding t v -> [Doc] -> [String]
              -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> f [Doc]
prettyBinding nm (Binding _ bt bs) dvs vs kt kv =
  h <*> traverse (\bd -> prettyBody nm bd dvs vs kt kv) bs
 where
 h = case bt of
       Explicit _ty -> pure id -- TODO: Type decl
       Implicit     -> pure id

prettyBindings :: Applicative f
               => [(Doc, Binding t v)] -> [Doc] -> [String]
               -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> f Doc
prettyBindings bs dvs vs kt kv =
  block . concat <$> traverse (\p -> uncurry prettyBinding p dvs vs kt kv) bs

prettyGuarded :: Applicative f => Guarded tm -> (tm -> f Doc) -> f Doc
prettyGuarded (Unguarded tm) k = (equals </>) <$> k tm
prettyGuarded (Guarded l)    k = align . sep <$> traverse (\(l', r) -> h <$> k l' <*> k r) l
 where
 h g b = text "|" <+> g <+> equals </> b

prettyBody :: Applicative f
           => Doc -> Body t v -> [Doc] -> [String]
           -> (t -> Int -> f Doc) -> (v -> Int -> f Doc) -> f Doc
prettyBody nm (Body ps gs wh) dvs vs kt kv =
  h <$> fpd <*> prettyGuarded gs (\(Scope e) -> prettyTerm e rest (-1) kt kv') <*> wrd
 where
 wl = length wh
 wrd | wl == 0   = pure Nothing
     | otherwise = Just <$> prettyBindings (zip wvs wh) wvs rest kt kw
 (bnd, fpd) = lambdaPatterns ps vs kt
 (_, (wvs, rest)) = first (map text) . splitAt wl <$> splitAt (HM.size bnd) vs

 h pd gd Nothing   = align $ nm <> pd <> nest 2 (softline <> gd)
 h pd gd (Just wd) = align $ nm <> pd <> nest 2 (softline <> gd)
                  <> nest 1 (line <> text "where" </> align wd)

 kv' (B (D i)) _  = pure $ dvs !! i
 kv' (B (P p)) _  = fromMaybe (error "PANIC: prettyBody: bad pattern variable reference")
                  $ pure . text <$> HM.lookup p bnd
 kv' (B (W i)) _  = pure $ wvs !! i
 kv' (F tm)    pr = prettyTerm tm rest pr kt kv

 kw (B p) = fromMaybe (error "PANIC: prettyBody: bad pattern variable reference")
          $ const . pure . text <$> HM.lookup p bnd
 kw (F v) = kv v

