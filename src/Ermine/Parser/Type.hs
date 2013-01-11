--------------------------------------------------------------------
-- |
-- Module    :  Ermine.Parser.Type
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for types
--------------------------------------------------------------------
module Ermine.Type.Parser
  ( tid
  ) where

import Control.Applicative
import Control.Lens
import Data.HashSet
import Ermine.Kind
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style

-- | The internal token style used for type variables
tid :: TokenParsing m => IdentifierStyle m
tid = haskellIdents
  & styleName  .~ "type variable"
  & styleReserved .~ keywords
