{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Ermine.Pretty.Kind
  (
  -- * Pretty Printing Kinds
    prettyHardKind
  , prettyKind
  , prettySchema
  ) where

import Bound
import Control.Applicative
import Ermine.Pretty
import Ermine.Syntax.Kind

-- $setup
-- >>> :set -XOverloadedStrings

prettyHardKind :: HardKind -> Doc
prettyHardKind Unboxed = "#"
prettyHardKind Star = "*"
prettyHardKind Constraint = "Γ"
prettyHardKind Rho = "ρ"
prettyHardKind Phi = "φ"

-- | Pretty print a 'Kind'
prettyKind :: Kind String -> Bool -> Doc
prettyKind (Var nm)     _ = text nm
prettyKind (HardKind h) _ = prettyHardKind h
prettyKind (Type k)     b = prettyKind k b
prettyKind (l :-> r)    b = parensIf b $ prettyKind l True <+> "->" <+> prettyKind r False

-- | Pretty print a 'Kind', using a fresh kind variable supply
--
-- You should have already removed any free variables from the variable set.
prettySchema :: Schema String -> [String] -> Doc
prettySchema (Schema hl b) vs = prettyKind (instantiate (pure . (ns!!)) b) False
 where
 (ns, _) = chooseNames (const False) hl vs
