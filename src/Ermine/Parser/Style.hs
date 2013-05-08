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
  , termIdentifier
  , typeIdentifier
  , kindIdentifier
  , op
  , operator
  , termCon
  , typeCon
  ) where

import Control.Applicative
import Control.Lens hiding (op)
import Data.HashSet as HashSet
import Data.Text (Text)
import Ermine.Parser.Keywords
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
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

-- | Parse a term identifier.
termIdentifier :: (Monad m, TokenParsing m) => m Text
termIdentifier = ident termIdent

-- | Parse a type identifier
typeIdentifier :: (Monad m, TokenParsing m) => m Text
typeIdentifier = ident typeIdent

-- | Parse a kind identifier
kindIdentifier :: (Monad m, TokenParsing m) => m Text
kindIdentifier = ident kindIdent

-- | The identifier style for operators.
--
-- TODO: make this more specific to each level?
op :: TokenParsing m => IdentifierStyle m
op = haskellOps

-- | Parse an operator.
operator :: (Monad m, TokenParsing m) => m Text
operator = ident op

capital :: TokenParsing m => IdentifierStyle m
capital = IdentifierStyle
        { _styleName = "capital"
        , _styleStart = upper
        , _styleLetter = alphaNum <|> oneOf "_'"
        , _styleReserved = HashSet.empty
        , _styleHighlight = Constructor
        , _styleReservedHighlight = ReservedConstructor
        }

termCon, typeCon :: TokenParsing m => IdentifierStyle m
termCon = capital & styleName .~ "term constructor"
typeCon = capital & styleName .~ "type constructor"
