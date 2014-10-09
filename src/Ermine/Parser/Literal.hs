--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for 'literal' terms or patterns
--------------------------------------------------------------------
module Ermine.Parser.Literal
  ( literal
  ) where

import Control.Applicative
import Ermine.Syntax.Literal
import Text.Parser.Token

-- | Parse a 'literal' number, string or character.
literal :: (Monad m, TokenParsing m) => m Literal
literal = either Integer Double <$> naturalOrDouble
      <|> String <$> stringLiteral
      <|> Char <$> charLiteral
