--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for terms
--------------------------------------------------------------------
module Ermine.Parser.Literal
  ( literal
  ) where

import Control.Applicative
import Ermine.Syntax.Literal
import Text.Parser.Token

literal :: (Monad m, TokenParsing m) => m Literal
literal = either (Int . fromIntegral) Double <$> naturalOrDouble
      <|> String <$> stringLiteral
      <|> Char <$> charLiteral
