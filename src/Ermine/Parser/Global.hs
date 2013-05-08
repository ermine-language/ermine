{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Dan Doel <dan.doel@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a parser for global names.
--
-- Note: most parsing goes through strings, which are later fixed
-- up into global names, but in some cases globals should be parsed
-- directly.
--------------------------------------------------------------------

module Ermine.Parser.Global
  ( globalIdent
  ) where

import Control.Applicative
import Ermine.Syntax.Global
import Text.Parser.Token

-- | Parse a global identifier with the given style.
-- TODO: package/module information
globalIdent :: (Monad m, TokenParsing m) => IdentifierStyle m -> m Global
globalIdent style = glob Idfix "ermine" "Ermine" <$> ident style
