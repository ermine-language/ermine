--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the style information for tokenizing of
-- various portions of the language.
--------------------------------------------------------------------
module Ermine.Parser.Style
  ( termIdent
  , typeIdent
  , kindIdent
  , op
  ) where

import Control.Lens hiding (op)
import Ermine.Parser.Keywords
import Text.Parser.Token
import Text.Parser.Token.Style

baseIdent, termIdent, typeIdent, kindIdent :: TokenParsing m => IdentifierStyle m

-- | The base identifier language for variables.
--
-- TODO: make keywords more specific to each level
baseIdent = haskellIdents & styleReserved .~ keywords

-- | The identifier style for term variables.
termIdent = baseIdent & styleName .~ "term variable"

-- | The identifier style for type variables.
typeIdent = baseIdent & styleName .~ "type variable"

-- | The identifier style for kind variables.
kindIdent = baseIdent & styleName .~ "kind variable"

-- | The identifier style for operators.
--
-- TODO: make this more specific to each level?
op :: TokenParsing m => IdentifierStyle m
op = haskellOps


