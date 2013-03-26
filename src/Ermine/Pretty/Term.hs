
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Ermine.Pretty.Term ( prettyHardTerm
                          , prettyTerm
                          ) where

import Bound
import Control.Applicative
import Ermine.Pretty
import Ermine.Pretty.Global
import Ermine.Pretty.Literal
import Ermine.Pretty.Pattern
import Ermine.Syntax.Term

prettyHardTerm :: HardTerm -> Doc
prettyHardTerm (Lit l)     = prettyLiteral l
prettyHardTerm (DataCon g) = prettyGlobal g
prettyHardTerm (Tuple n)   = parens . text $ replicate (n-1) ','
prettyHardTerm Hole        = text "?{}"

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
 (n, fpd) = lambdaPatterns ps vars kt
 (used, vars') = splitAt n vars
 h pd bd = parensIf (prec >= 0) $ pd <+> text "->" <+> bd
 kv' (B i) _ = pure . text $ used !! i
 kv' (F t) p = prettyTerm t vars' p kt kv
prettyTerm _ _ _ _ _ = error "TODO"
-- prettyTerm (Case d alts)  vars prec kt kv = undefined
-- prettyTerm (Let bs e)     vars prec kt kv = undefined
