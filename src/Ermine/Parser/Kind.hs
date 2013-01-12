--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Parser.Kind
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for kinds.
--------------------------------------------------------------------
module Ermine.Parser.Kind
  ( kind
  ) where

import Control.Applicative
import Control.Lens
import Ermine.Parser.Keywords
import Ermine.Syntax.Kind
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

-- | The internal token style used for kind variables
kid :: TokenParsing m => IdentifierStyle m
kid = haskellIdents
  & styleName  .~ "kind variable"
  & styleReserved .~ keywords

-- | Parse an atomic kind (everything but arrows)
kind0 :: (Monad m, TokenParsing m) => m (Kind String)
kind0 = parens kind
    <|> star <$ symbol "*"
    <|> rho <$ reserve kid "rho"
    <|> rho <$ reserve kid "ρ"
    <|> phi <$ reserve kid "phi"
    <|> phi <$ reserve kid "φ"
    <|> constraint <$ reserve kid "constraint"
    <|> constraint <$ reserve kid "Γ"
    <|> Var <$> ident kid

-- | Parse a 'Kind'.
kind :: (Monad m, TokenParsing m) => m (Kind String)
kind = chainr1 kind0 ((:->) <$ symbol "->")
