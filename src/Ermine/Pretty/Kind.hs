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
  , prettyKind_
  , prettySchema
  ) where

import Bound
import Control.Applicative
import Ermine.Pretty
import Ermine.Syntax.Kind
import Data.Functor.Identity

-- $setup
-- >>> :set -XOverloadedStrings

prettyHardKind :: HardKind -> Doc
prettyHardKind Star = "*"
prettyHardKind Constraint = "Γ"
prettyHardKind Rho = "ρ"
prettyHardKind Phi = "φ"

-- | Pretty print a 'Kind', using a helper to print free variables
prettyKind :: Applicative f => Kind a -> Bool -> (a -> Bool -> f Doc) -> f Doc
prettyKind (Var a)      b k = k a b
prettyKind (HardKind h) _ _ = pure $ prettyHardKind h
prettyKind (l :-> r)    b k = go <$> prettyKind l True k <*> prettyKind r False k where
  go x y = parensIf b (x <+> "->" <+> y)

prettyKind_ :: Kind String -> Doc
prettyKind_ k = runIdentity $ prettyKind k False $ const . Identity . pretty

-- | Pretty print a 'Kind', using a fresh kind variable supply and a helper to print free variables
--
-- You should have already removed any free variables from the variable set.
prettySchema :: Applicative f => Schema a -> [String] -> (a -> Bool -> f Doc) -> f Doc
prettySchema (Schema _ b) xs k = prettyKind (fromScope b) False $ \ v p -> case v of
  B i -> pure $! text (xs !! i)
  F a -> k a p
